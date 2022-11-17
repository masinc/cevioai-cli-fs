module Schema

open System.Collections.Generic
open System.Text.Json
open System.Text.Json.Serialization

type InputSchemaComponent() =
    member val name: string = null with get, set
    member val value: uint32 option = None with get, set

    interface Interface.IComponent with
        member this.name = this.name
        member this.value = this.value


type InputSchema() =
    member val name: string = null with get, set
    member val text: string = null with get, set
    member val volume: uint32 option = None with get, set
    member val alpha: uint32 option = None with get, set
    member val speed: uint32 option = None with get, set
    member val tone: uint32 option = None with get, set
    member val tone_scale: uint32 option = None with get, set
    member val components: IEnumerable<InputSchemaComponent> option = None with get, set

    interface Interface.ITalkable with
        member this.name = this.name
        member this.volume = this.volume
        member this.alpha = this.alpha
        member this.speed = this.speed
        member this.tone = this.tone
        member this.tone_scale = this.tone_scale

        member this.components =
            this.components
            |> Option.map (fun c -> c |> Seq.cast<Interface.IComponent>)

    static member from_json(json: string) : InputSchema = JsonSerializer.Deserialize json
