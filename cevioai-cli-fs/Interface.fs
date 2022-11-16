module Interface

open System.Collections.Generic

type IComponent =
    abstract name: string
    abstract value: uint32 option

type ITalkable =
    abstract name: string
    abstract volume: uint32 option
    abstract alpha: uint32 option
    abstract speed: uint32 option
    abstract tone: uint32 option
    abstract tone_scale: uint32 option
    abstract components: IEnumerable<IComponent> option
