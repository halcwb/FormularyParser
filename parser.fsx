#I "./packages/ExcelProvider/lib"
#r "ExcelProvider.dll"

#I "./packages/FSharp.Data/lib/net40"
#r "FSharp.Data.dll"

#I "./packages/HtmlAgilityPack/lib/net40"
#r "HtmlAgilityPack.dll"

#time

open System

open FSharp.ExcelProvider

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__


module Double =

    let tryParse s =
        let (b, n) = Double.TryParse(s)
        if b then Some n else None

module Char =

    let letters = [|'a'..'z'|]

    let capitals = [|'A'..'Z'|]

    let isCapital c = capitals |> Seq.exists ((=) c)


module String =

    let apply f (s: string) = f s

    let nullOrEmpty = apply String.IsNullOrEmpty

    let notNullOrEmpty = nullOrEmpty >> not

    let splitAt s1 (s2: string) =
        let cs = s1 |> Array.ofSeq
        s2.Split(cs)

    let arrayConcat (cs : char[]) = String.Concat(cs)



type Drugs = ExcelFile<"Formularium_NicuPicu.xlsx">
type Doses = ExcelFile<"FORMULARIUM_doseringen.xlsx">

let drugs = new Drugs()
let doses = new Doses()

type Medicament =
    {
        GPK: string
        MainGroup: string
        SubGroup: string
        Therapeutic: string
        Label: string
        Generic: string list
        Quantity: float list
        QuantityUnit: string
        Shape: string
        ShapeUnit: string
        Route: string
        DoseUnit: string
    }

let emptyMed =
    {
        GPK = ""
        MainGroup = ""
        SubGroup = ""
        Therapeutic = ""
        Label = ""
        Generic = []
        Quantity = []
        QuantityUnit = ""
        Shape = ""
        ShapeUnit = ""
        Route = ""
        DoseUnit = ""
    }

let parseQuantity s =
    if s |> String.IsNullOrEmpty then []
    else
        s
        |> String.splitAt "/"
        |> Array.map(fun s ->
            s
            |> Array.ofSeq
            |> Array.filter (Char.isCapital >> not)
            |> String.arrayConcat)
        |> Array.filter String.notNullOrEmpty
        |> Array.map Double.tryParse
        |> Array.filter Option.isSome
        |> Array.map Option.get
        |> Array.toList

let parseGeneric s =
    s
    |> String.splitAt " "
    |> Array.head
    |> String.splitAt "/"
    |> List.ofArray

for r in drugs.Data do
    r.``Sterkte$`` |> parseQuantity
    |> printfn "%A"

[ for r in drugs.Data do
    if r.GPKcode |> String.notNullOrEmpty then
        yield { emptyMed with GPK = r.GPKcode
                              MainGroup = r.Hoofdgroep
                              SubGroup = r.Subgroep
                              Therapeutic = r.``Therapeutische groep``
                              Label = r.Stofnaam
                              Generic = r.Stofnaam |> parseGeneric
                              Quantity = r.``Sterkte$`` |> parseQuantity
                              QuantityUnit = r.``Eenheid stof``
                              Shape = r.``Farm Vorm``
                              ShapeUnit = r.``Eenheid product``
                              Route = r.Toedieningsweg
                              DoseUnit = r.``Eenheid voorschrijven`` } ]
|> List.distinct
// check if num of generics eqs num of quantities
|> List.filter (fun d ->
    d.Quantity |> List.length = 0 ||
    d.Generic |> List.length = (d.Quantity |> List.length))
// check dose unit is either quantity unit or shape unit
// or dose unit is 'STUK'
|> List.filter (fun d ->
    let check =
        d.DoseUnit = "STUK" ||
        d.DoseUnit = d.QuantityUnit ||
        d.DoseUnit = d.ShapeUnit
    if check |> not then printfn "%A" d
    check)
// check uniqueness of GPK
|> List.length = (drugs.Data
                  |> Seq.filter (fun r -> r.GPKcode |> String.notNullOrEmpty)
                  |> Seq.map (fun r -> r.GPKcode)
                  |> Seq.distinct
                  |> Seq.length)


open FSharp.Data
open FSharp.Data.JsonExtensions

// get all medications from Kinderformularium
let kinderFormUrl = "https://www.kinderformularium.nl/geneesmiddelen.json"

type Schedule = 
    {
        Target: string
        Value: string
        Unit: string
        Frequency: string
    }

type Route = 
    {
        Name: string
        Schedules: Schedule list
    }

type Dose = 
    {
        Indication: string
        Routes: Route list
    }

type Drug = 
    { 
        Id: string
        Generic: string
        Brand: string 
        Doses: Dose list
    }

let createDrug id gen br = 
    {
        Id = id
        Generic = gen
        Brand = br
        Doses = [] 
    }

let medications =
    let res = JsonValue.Load(kinderFormUrl)
    [ for v in res do
        yield createDrug (v?id.AsString()) (v?generic_name.AsString()) (v?branded_name.AsString()) ]


let drugUrl = sprintf "https://www.kinderformularium.nl/geneesmiddel/%s/%s?nolayout"

let getDoc get id gen = drugUrl id gen |> get

let getDocSync = getDoc HtmlDocument.Load

let getDocAsync = getDoc HtmlDocument.AsyncLoad

let getParent d n =
    d
    |> HtmlDocument.descendants false (HtmlNode.elements >> Seq.exists ((=) n))
    |> Seq.head

let getItemType desc v d =
    d |> desc true (HtmlNode.hasAttribute "itemType" v)

let getItemTypeFromDoc = getItemType HtmlDocument.descendants

let getItemTypeFromNode = getItemType HtmlNode.descendants

let getIndications d = d |> getItemTypeFromDoc "http://schema.org/MedicalIndication"

let doseSchedule n = n |> getItemTypeFromNode  "http://schema.org/DoseSchedule"

let getItemProp v n =
    n |> HtmlNode.descendantsAndSelf true (HtmlNode.hasAttribute "itemprop" v)

let getItemPropString v n =
    match n |> getItemProp v
            |> List.ofSeq with
    | h::_ -> h |> HtmlNode.innerText
    | _ -> ""


for med in (medications |> List.take (medications.Length)) do
    printfn "\n\n========== %s ===========\n" med.Generic
    let doc = getDocSync med.Id med.Generic

    for ind in doc |> getItemTypeFromDoc "http://schema.org/MedicalIndication" do
        let name = ind |> getItemPropString "name"
        printfn "\n-- Indication: %A" name
        let ind' =
            ind
            |> getParent doc
            |> getParent doc

        for r in ind' |> getItemProp "administrationRoute" do
            let route = r |> getItemPropString "administrationRoute"
            printfn "\n-- Route: %A" route
            let r' = r |> getParent doc

            for dose in r' |> getItemTypeFromNode "http://schema.org/DoseSchedule" do
                let targetPop = dose |> getItemPropString "targetPopulation"
                let doseVals = dose |> getItemPropString "doseValue"
                let doseUnits = dose |> getItemPropString "doseUnit"
                let freq = dose |> getItemPropString "frequency"

                printfn "Target Population: %s" targetPop
                printfn "Dose: %s %s %s" doseVals doseUnits freq

let addDoses (m: Drug) = async {
        let! doc = getDocAsync m.Id m.Generic
        let getPar = getParent doc
        return { m with Doses = [ 
            for i in doc |> getItemTypeFromDoc "http://schema.org/MedicalIndication" do
                let n = i |> getItemPropString "name"
                let i' = i |> getPar |> getPar
                yield { Indication = n; Routes = [ 
                    for r in i' |> getItemProp "administrationRoute" do
                        let n = r |> getItemPropString "administrationRoute"
                        let r' = r |> getPar
                        yield { Name = n; Schedules = [ 
                            for s in r' |> getItemTypeFromNode "http://schema.org/DoseSchedule" do
                                let tp = s |> getItemPropString "targetPopulation"
                                let dv = s |> getItemPropString "doseValue"
                                let du = s |> getItemPropString "doseUnit"
                                let fr = s |> getItemPropString "frequency"
                                yield { Target = tp; Value = dv; Unit = du; Frequency = fr }
                        ]}  
                ]}
            ] 
        }
    }

let ms =
    medications
    |> List.map addDoses
    |> Async.Parallel 
    |> Async.RunSynchronously

ms 
|> Array.toList
|> List.collect (fun m -> m.Doses)
|> List.collect (fun d -> d.Routes)
|> List.collect (fun r -> r.Schedules)
|> List.map (fun s -> s.Frequency)
|> List.distinct
|> List.iter (printfn "%s")

