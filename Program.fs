
open System

let parseEnum<'T when 'T :> Enum> (value: string) =
    System.Enum.Parse(typedefof<'T>, value, true) :?> 'T

type StrEnum<'TEnum when 'TEnum :> Enum>(enumToStringMap : 'a -> string) =

    let getAllCases =
        fun () ->
            System.Enum.GetNames(typeof<'TEnum>)

    let enumNameToStringLookup
        =
        getAllCases()
        |> Array.map (
            fun case ->
                (case, (enumToStringMap (parseEnum<'TEnum> case)))
        )
        |> dict

    let enumStringToNameLookup
        =
        getAllCases()
        |> Array.map (
            fun case ->
                ((enumToStringMap (parseEnum<'TEnum> case)), case)
        )
        |> dict

    member __.GetEnum (s: string): 'TEnum option =
        if not (enumStringToNameLookup.ContainsKey(s))
        then None

        else
        let enumName = enumStringToNameLookup[s]
        Some (
            parseEnum<'TEnum> enumName
        )

    member __.GetString (e: 'TEnum): string =
        enumNameToStringLookup[e.ToString()]

(*
    **************************************
    Some example code
    **************************************
*)

type DateRangeType =
    | Created = 1
    | Processing = 2
    | Completed = 3
    | Cancelled = 4

let converter = StrEnum<DateRangeType>(
    fun e ->
        match e with 
        | DateRangeType.Created -> "Created"
        | DateRangeType.Processing -> "Processing"
        | DateRangeType.Completed -> "Completed"
        | DateRangeType.Cancelled -> "Cancelled"
)

let findStringForDateRangeType (state: DateRangeType) =
    converter.GetString(state)

printf $"Created: {findStringForDateRangeType(DateRangeType.Created)}\n"
printf $"Cancelled: {findStringForDateRangeType(DateRangeType.Cancelled)}\n"

let findDateRangeTypeForString (state: string) =
    converter.GetEnum(state)

let value =findDateRangeTypeForString("Created") 
match value with
| Some x ->
    match x with 
    | DateRangeType.Created -> printf $"Created: {x}\n"
    | _ -> printf "Created: Wrong DateRangeType found."
| None -> printf "Did not find DateRangeType.Created"

let value2 =findDateRangeTypeForString("Processing") 
match value2 with
| Some x -> 
    match x with 
    | DateRangeType.Processing -> printf $"Processing: {x}\n"
    | _ -> printf "Processing: Wrong DateRangeType found.\n"
| None -> printf "Did not find DateRangeType.Processing\n"

let value3 =findDateRangeTypeForString("BadValue") 
match value3 with
| Some x -> printf "BadValue: Wrong DateRangeType found.\n"
| None -> printf "Did not find DateRangeType.BadValue\n"
