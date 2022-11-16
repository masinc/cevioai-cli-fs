open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Unicode
open CommandLine
open CeVIO.Talk.RemoteService2
open System.Linq
open CommandLine.Text
open Interface

[<RequireQualifiedAccess>]
type InputFormat =
    | auto = 0
    | json = 1

[<RequireQualifiedAccess>]
type OutputFormat =
    | text = 0
    | json = 1


type ListResultComponent() =
    member val id = "" with get, set
    member val name = "" with get, set
    member val value = (uint) 0 with get, set

    static member from(tc2: TalkerComponent2) =
        ListResultComponent(id = tc2.Id, name = tc2.Name, value = tc2.Value)

type ListResult() =
    member val name = "" with get, set
    member val components: ListResultComponent [] = [||] with get, set

    static member from(talker: Talker2) =
        let cs =
            talker.Components.ToArray()
            |> Array.map ListResultComponent.from

        ListResult(name = talker.Cast, components = cs)

[<Verb("list")>]
type CliList =
    { [<Option('v', "verbose", Default = false)>]
      verbose: bool
      [<Option('o', "output", Default = 0)>]
      output: OutputFormat }
    member this.run() =
        match ServiceControl2.StartHost false with
        | HostStartResult.Succeeded ->
            match (this.output, this.verbose) with
            // output=text verbose=false
            | (OutputFormat.text, false) ->
                TalkerAgent2.AvailableCasts
                |> Array.iter (fun v -> printfn $"{v}")

                0
            // output=text verbose=true
            | (OutputFormat.text, true) ->
                TalkerAgent2.AvailableCasts
                |> Seq.map (fun name -> Talker2(cast = name))
                |> Seq.map ListResult.from
                |> Seq.iter (fun r ->
                    let components_names =
                        r.components
                        |> Array.map (fun c -> c.name)
                        |> String.concat ","

                    printfn $"{r.name}\t{components_names}")

                0
            // output=json
            | (OutputFormat.json, _) ->
                let r =
                    TalkerAgent2.AvailableCasts
                    |> Seq.map (fun name -> Talker2(cast = name))
                    |> Seq.map ListResult.from
                    |> Seq.toArray

                let opt = JsonSerializerOptions()
                opt.Encoder <- JavaScriptEncoder.Create(UnicodeRanges.All)
                opt.WriteIndented <- true
                let s = JsonSerializer.Serialize(r, opt)
                printfn $"{s}"
                0
            | _ -> failwith "unreachable"
        | n -> (int) n

let get_talker (talkable: ITalkable) =
    let t = Talker2(cast = talkable.name)

    t.Volume <-
        (t.Volume, talkable.volume)
        ||> Option.defaultValue

    t.Alpha <- (t.Alpha, talkable.alpha) ||> Option.defaultValue
    t.Speed <- (t.Speed, talkable.speed) ||> Option.defaultValue
    t.Tone <- (t.Tone, talkable.tone) ||> Option.defaultValue

    t.ToneScale <-
        (t.ToneScale, talkable.tone_scale)
        ||> Option.defaultValue

    match talkable.components with
    | Some (cs) ->
        cs
        |> Seq.iter (fun c ->
            match c.value with
            | Some (value) ->
                let tc = t.Components.ByName c.name
                tc.Value <- value
            | None -> ())
    | None -> ()

    t


[<Verb("play")>]
type CliPlay() =
    interface ITalkable with
        member this.name = this.name
        member this.volume = this.volume
        member this.alpha = this.alpha
        member this.speed = this.speed
        member this.tone = this.tone
        member this.tone_scale = this.tone_scale
        member this.components = None

    [<Option('n', "name", Required = true, HelpText = "talker name")>]
    member val name: string = null with get, set

    [<Option('v', "volume", HelpText = "volume(音量). value: 0-100")>]
    member val volume: uint32 option = None with get, set

    [<Option('a', "alpha", HelpText = "alpha(声質). value: 0-100")>]
    member val alpha: uint32 option = None with get, set

    [<Option('s', "speed", HelpText = "speed(話す速さ). value: 0-100")>]
    member val speed: uint32 option = None with get, set

    [<Option("tone", HelpText = "tone(声の高さ). value: 0-100")>]
    member val tone: uint32 option = None with get, set

    [<Option("tone-scale", HelpText = "tone scale(抑揚). value: 0-100")>]
    member val tone_scale: uint32 option = None with get, set

    [<Option('t', "text", Group = "input")>]
    member val text: string = null with get, set

    [<Option('f', "file", Group = "input", HelpText = "specify auto, json")>]
    member val file: string = null with get, set

    member this.run() =
        match ServiceControl2.StartHost false with
        | HostStartResult.Succeeded ->
            let talker = get_talker this

            let text =
                if this.text <> null then
                    this.text
                elif this.file <> null then
                    System.IO.File.ReadAllText this.file
                else
                    failwith "unreachable"

            let state = talker.Speak text
            state.Wait()

            if state.IsSucceeded then 0 else 1
        | n -> (int) n

[<Verb("play-from")>]
type CliPlayFrom() =
    [<Option('f', "file", Group = "input")>]
    member val file: string = null with get, set

    [<Option('F', "format", HelpText = "specify input format: auto, json")>]
    member val format: InputFormat = InputFormat.auto with get, set

    member this.run() =
        let input =
            if this.file <> null then
                let text =
                    System.IO.File.ReadAllText this.file

                match this.format with
                | InputFormat.auto ->
                    let ext =
                        System.IO.Path.GetExtension(this.file).ToUpper()

                    match ext with
                    | ".JSON" -> Schema.InputSchema.from_json text
                    | _ -> failwithf $"The extension was not supported ({ext}). Please specify input argument"
                | InputFormat.json -> Schema.InputSchema.from_json text
                | _ -> failwithf "unreachable"
            else
                failwithf "unreachable"

        let talker = get_talker input
        let state = talker.Speak input.text
        state.Wait()

        if state.IsSucceeded then 0 else 1

[<Verb("save")>]
type CliSave() =
    interface ITalkable with
        member this.name = this.name
        member this.volume = this.volume
        member this.alpha = this.alpha
        member this.speed = this.speed
        member this.tone = this.tone
        member this.tone_scale = this.tone_scale
        member this.components = None

    [<Option('n', "name", Required = true, HelpText = "talker name")>]
    member val name: string = null with get, set

    [<Option('v', "volume", HelpText = "volume(音量). value: 0-100")>]
    member val volume: uint32 option = None with get, set

    [<Option('a', "alpha", HelpText = "alpha(声質). value: 0-100")>]
    member val alpha: uint32 option = None with get, set

    [<Option('s', "speed", HelpText = "speed(話す速さ). value: 0-100")>]
    member val speed: uint32 option = None with get, set

    [<Option("tone", HelpText = "tone(声の高さ). value: 0-100")>]
    member val tone: uint32 option = None with get, set

    [<Option("tone-scale", HelpText = "tone scale(抑揚). value: 0-100")>]
    member val tone_scale: uint32 option = None with get, set

    [<Option('t', "text", Group = "input")>]
    member val text: string = null with get, set

    [<Option('f', "file", Group = "input")>]
    member val file: string = null with get, set

    [<Option('o', "output", Required = true, HelpText = "specifies the output path of the wav file")>]
    member val output: string = null with get, set

    member this.run() =
        match ServiceControl2.StartHost false with
        | HostStartResult.Succeeded ->
            let talker = get_talker this

            let text =
                if this.text <> null then
                    this.text
                elif this.file <> null then
                    System.IO.File.ReadAllText this.file
                else
                    failwith "unreachable"

            if talker.OutputWaveToFile(text, this.output) then
                0
            else
                1
        | n -> (int) n

[<EntryPoint>]
let main args =
    let result =
        Parser.Default.ParseArguments<CliList, CliPlay, CliSave, CliPlayFrom> args

    match result with
    | :? CommandLine.Parsed<obj> as command ->
        match command.Value with
        | :? CliList as opts -> opts.run ()
        | :? CliPlay as opts -> opts.run ()
        | :? CliSave as opts -> opts.run ()
        | :? CliPlayFrom as opts -> opts.run ()
        | _ -> failwith "unreachable"
    | :? CommandLine.NotParsed<obj> -> 1
    | _ -> failwith "unreachable"
