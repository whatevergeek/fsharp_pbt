module DiamondProperties

open System
open FsCheck
open FsCheck.Xunit

let mutable count: int = 0

type Letters = 
    static member Char = 
        Arb.Default.Char()
        |> Arb.filter(fun c -> 'A' <= c && c <= 'Z')
    
type DiamondPropertyAttribute () = 
    inherit PropertyAttribute(Arbitrary = [|typeof<Letters>|])

[<Property>]
let ``Diamond is non-empty`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual)

[<Property(Arbitrary = [|typeof<Letters>|])>]
let ``Diamond is non-empty - Version 3`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual)

[<DiamondPropertyAttribute>]
let ``Diamond is non-empty - Version 2`` (letter: char) =
    count <- count + 1
    let actual = Diamond.make letter
    not (String.IsNullOrWhiteSpace actual)

