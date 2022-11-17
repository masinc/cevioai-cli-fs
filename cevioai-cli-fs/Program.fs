open System.Text.Encodings.Web
open System.Text.Json
open System.Text.Unicode
open CommandLine
open CeVIO.Talk.RemoteService2
open System.Linq
open CommandLine.Text
open Interface
open Schema

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

type ITalkable with
    member this.get_talker() =
        let t = Talker2(cast = this.name)
        t.Volume <- (t.Volume, this.volume) ||> Option.defaultValue
        t.Alpha <- (t.Alpha, this.alpha) ||> Option.defaultValue
        t.Speed <- (t.Speed, this.speed) ||> Option.defaultValue
        t.Tone <- (t.Tone, this.tone) ||> Option.defaultValue

        t.ToneScale <-
            (t.ToneScale, this.tone_scale)
            ||> Option.defaultValue

        match this.components with
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
        let talker = this.get_talker ()

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
        let talker = this.get_talker ()

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

type InputType =
    | File of file: string
    | Stdin

type InputType with
    member this.get_input(format: InputFormat) =
        match format, this with
        | InputFormat.json, File file ->
            System.IO.File.ReadAllText file
            |> InputSchema.from_json
        | InputFormat.json, Stdin ->
            System.Console.In.ReadToEnd()
            |> InputSchema.from_json
        | InputFormat.auto, File file ->
            let ext =
                System.IO.Path.GetExtension(file).ToLower()

            match ext with
            | ".json" ->
                System.IO.File.ReadAllText file
                |> InputSchema.from_json
            | _ -> failwithf "The file has a non-supported extension"
        | InputFormat.auto, Stdin -> failwithf "Stdin requires the format argument to be specified"
        | _ -> failwith "todo"


[<Verb("play-from")>]
type CliPlayFrom() =
    [<Value(0, Default = "-", Required = false)>]
    member val file: string = null with get, set

    [<Option('f', "format", HelpText = "specify input format: auto, json")>]
    member val format: InputFormat = InputFormat.auto with get, set

    member this.run() =
        let data =
            let input_type =
                match this.file with
                | null -> failwith "unreachable"
                | "-" -> Stdin
                | s -> File s

            input_type.get_input this.format

        let talker = data.get_talker ()
        let state = talker.Speak data.text
        state.Wait()

        if state.IsSucceeded then 0 else 1

[<Verb("save-from")>]
type CliSaveFrom() =
    [<Value(0, Default = "-", Required = false)>]
    member val file: string = null with get, set

    [<Option('f', "format", HelpText = "specify input format: auto, json")>]
    member val format: InputFormat = InputFormat.auto with get, set

    [<Option('o', "output", Required = true, HelpText = "specifies the output path of the wav file")>]
    member val output: string = null with get, set

    member this.run() =
        let data =
            let input_type =
                match this.file with
                | null -> failwith "unreachable"
                | "-" -> Stdin
                | s -> File s

            input_type.get_input this.format

        let talker = data.get_talker ()

        if talker.OutputWaveToFile(data.text, this.output) then
            0
        else
            1

[<EntryPoint>]
let main args =
    let result =
        Parser.Default.ParseArguments<CliList, CliPlay, CliSave, CliPlayFrom, CliSaveFrom> args

    match result with
    | :? CommandLine.Parsed<obj> as command ->
        match ServiceControl2.StartHost false with
        | HostStartResult.Succeeded ->
            match command.Value with
            | :? CliList as opts -> opts.run ()
            | :? CliPlay as opts -> opts.run ()
            | :? CliSave as opts -> opts.run ()
            | :? CliPlayFrom as opts -> opts.run ()
            | :? CliSaveFrom as opts -> opts.run ()
            | _ -> failwith "unreachable"
        | r -> (int) r
    | :? CommandLine.NotParsed<obj> -> 1
    | _ -> failwith "unreachable"
