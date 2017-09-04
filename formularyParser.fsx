﻿
#time


#I "./packages/FSharp.Data/lib/net40"
#r "FSharp.Data.dll"

#I "./packages/HtmlAgilityPack/lib/net40"
#r "HtmlAgilityPack.dll"

#I "./packages/Newtonsoft.Json/lib/net40"
#r "Newtonsoft.Json.dll"


open System


Environment.CurrentDirectory <- __SOURCE_DIRECTORY__



module Memoization =

    open System.Collections.Generic
    
    let memoize f =
        let cache = ref Map.empty
        fun x ->
            match (!cache).TryFind(x) with
            | Some r -> r
            | None ->
                let r = f x
                cache := (!cache).Add(x, r)
                r

    let memoizeOne f = 
        let dic = new Dictionary<_, _>()
        let memoized par =
            if dic.ContainsKey(par) then 
                dic.[par]
            else
                let result = f par
                dic.Add(par, result)
                result

        memoized

    let memoize2Int f =
        let dic = new Dictionary<int * int, _>()
        let memoized p1 p2 =
            let hash = p1.GetHashCode(), p2.GetHashCode()
            if dic.ContainsKey(hash) then 
                dic.[hash]
            else
                let result = f p1 p2
                dic.Add(hash, result)
                result

        memoized



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Double =

    open System

    let parse s = 
        try Double.Parse(s, Globalization.CultureInfo.InvariantCulture)
        with
        | _ -> 
            sprintf "Could not parse '%s' to double" s
            |> failwith

    let tryParse s =
        let (b, n) = Double.TryParse(s, Globalization.NumberStyles.Any, Globalization.CultureInfo.InvariantCulture) //Double.TryParse(s)
        if b then Some n else None

    let stringToFloat32 s =
        match s |> tryParse with
        | Some v -> float32 v
        | None -> 0.f



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Char =

    let letters = [|'a'..'z'|]

    let capitals = [|'A'..'Z'|]

    let isCapital c = capitals |> Seq.exists ((=) c)



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module String =

    let apply f (s: string) = f s

    let length s = (s |> apply id).Length

    let nullOrEmpty = apply String.IsNullOrEmpty

    let notNullOrEmpty = nullOrEmpty >> not

    let trim s = (s |> apply id).Trim()

    let splitAt (s1: string) (s2: string) =
        s2.Split([|s1|], StringSplitOptions.None)

    let arrayConcat (cs : char[]) = String.Concat(cs)

    let toLower s = (s |> apply id).ToLower()

    let equalsCapsInsens s1 s2 =
        s1 |> toLower = (s2 |> toLower)

    let _startsWith eqs s1 s2 = 
        if s2 |> String.length >= (s1 |> String.length) then
            s2.Substring(0, (s1 |> String.length)) |> eqs s1
        else false

    let startsWith = _startsWith (=)

    let startsWithCaseInsens = _startsWith equalsCapsInsens 

    let replace (s1: String) s2 s = (s |> apply id).Replace(s1, s2)

    let contains s1 s2 = (s2 |> apply id).Contains(s1)



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Regex =

    open System.Text.RegularExpressions
    
    let regex s = new Regex(s)

    let regexMatch m s = (m |> regex).Match(s)

    [<Literal>]
    let alphaRegex = "(?<Alpha>[a-zA-Z]*)"

    [<Literal>]
    let numRegex = "(?<Numeric>[0-9]*)"

    [<Literal>]
    let floatRegex = "(?<Float>[-+]?(\d*[.])?\d+)"

    let matchFloat s = 
        (s |> regexMatch floatRegex).Groups.["Float"].Value

    let matchAlpha s =
        (s |> regexMatch alphaRegex).Groups.["Alpha"].Value

    let matchFloatAlpha s =
        let grps = (floatRegex + alphaRegex |> regex).Match(s).Groups
        grps.["Float"].Value, grps.["Alpha"].Value



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module File =
    
    open System
    open System.IO

    [<Literal>]
    let cachePath = "pediatric.cache"

    let writeTextToFile path text =
        File.WriteAllText(path, text) 

    let exists path =
        File.Exists(path)

    let readAllLines path = File.ReadAllLines(path)



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Json =

    open System
    open System.IO
    open System.Text
    open Newtonsoft.Json

    ///
    let serialize x =
        JsonConvert.SerializeObject(x)


    let deSerialize<'T> (s: string) =
        JsonConvert.DeserializeObject<'T>(s)



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Drug =

    
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Frequency =

        type Frequency =
            | Frequency of Quantity
            | PRN of Quantity
            | AnteNoctum
            | Once
            | Bolus
        and Quantity =
            {
                Min : int
                Max : int
                Time : int
                Unit : string
            }

        let isValid = function
            | Frequency (fr) 
            | PRN (fr) -> 
                fr.Max > 0 && fr.Time > 0 && fr.Unit 
                |> String.notNullOrEmpty
            | _ -> true


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module MinMax =

        type MinMax = { Min : float Option; Max : float Option }


    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Target =

        type Target = 
            | Target of TargetType * TargetAge * TargetWeight
        and TargetType = 
            | AllType
            | Girl
            | Boy
            | Neonate
            | Aterm
            | Premature
        and TargetAge =
            | AllAge
            | Age of QuantityUnit Option * QuantityUnit Option 
            | Pregnancy of QuantityUnit Option * QuantityUnit Option  
            | PostConc of QuantityUnit Option * QuantityUnit Option 
        and TargetWeight =
            | AllWeight
            | Weight of QuantityUnit Option * QuantityUnit Option 
            | BirthWeight of QuantityUnit Option * QuantityUnit Option  
        and QuantityUnit = { Quantity : float; Unit : string }

        let createQuantity v u = { Quantity = v; Unit = u }

        let getTarget (Target(tt , ta , tw)) = (tt , ta , tw)

        let getQuantityUnit { Quantity = q; Unit = u} = (q, u)


    type Schedule = 
        {
            TargetText : string
            Target : Target.Target
            FrequencyText : string
            Frequency : Frequency.Frequency Option
            ValueText : string
            Value : MinMax.MinMax Option
            Unit : string
        }

    type Route = 
        {
            Name : string
            Schedules : Schedule list
        }

    type Dose = 
        {
            Indication : string
            Routes : Route list
        }

    type Drug = 
        { 
            Id : string
            Atc: string
            Generic : string
            Brand : string 
            Doses : Dose list
        }

    let createDrug id atc gen br = 
        {
            Id = id
            Atc = atc
            Generic = gen
            Brand = br
            Doses = [] 
        }



[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module FormularyParser =
    
    open System

    module FrequencyParser =

        let Frequency = Drug.Frequency.Frequency
        let AnteNoctum = Drug.Frequency.AnteNoctum
        let PRN = Drug.Frequency.PRN
        let Once = Drug.Frequency.Once

        let printFreqMap (ds : Drug.Drug []) =
            let tab = "    "
            printfn ""
            printfn "let freqMap ="
            printfn "%s[" tab
            ds//|> Seq.length
            |> Array.toList
            |> List.collect (fun m -> m.Doses)
            |> List.collect (fun d -> d.Routes)
            |> List.collect (fun r -> r.Schedules)
            |> List.map (fun s -> s.FrequencyText)
            |> List.distinct
            |> List.sort //|> List.length
            |> List.iter (printfn "%s%s(\"%s\", Frequency({ Min = 0; Max = 0; Time = 0; Unit = \"\"}) |> Some)" tab tab)
            printfn "%s]" tab
            printfn ""

        let freqMap =
            [
                ("", None)
                (" 0,5-2 uur voorafgaand aan reis innemen, ZN elke 6-8 uur tijdens reis herhalen ", PRN({ Min = 3; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 1 dd ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "day"}) |> Some)
                (" 1 dd 's avonds ", AnteNoctum |> Some)
                (" 1 dd of 1 x per 2 dagen ", Frequency({ Min = 1; Max = 2; Time = 2; Unit = "day"}) |> Some)
                (" 1 x per 2 weken ", Frequency({ Min = 1; Max = 1; Time = 2; Unit = "week"}) |> Some)
                (" 1 x per 4 weken ", Frequency({ Min = 1; Max = 1; Time = 4; Unit = "week"}) |> Some)
                (" 1 x per maand ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "month"}) |> Some)
                (" 1 x per week ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "week"}) |> Some)
                (" 1-2 dd ", Frequency({ Min = 1; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" 1-2 dd. ", Frequency({ Min = 1; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" 1-3 dd ", Frequency({ Min = 1; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" 1-3 dd, ", Frequency({ Min = 1; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" 1-3 dd, maximaal 4-6 dd ", Frequency({ Min = 1; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" 1-3 maal daags ", Frequency({ Min = 1; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" 1-3 maal per week ", Frequency({ Min = 1; Max = 3; Time = 1; Unit = "week"}) |> Some)
                (" 1-4 dd ", Frequency({ Min = 1; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 1-4 maal daags ", Frequency({ Min = 1; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 2 dd ", Frequency({ Min = 2; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" 2 dd (' s ochtends en 's avonds) ", Frequency({ Min = 2; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" 2 dd (om de 8 uur) ", Frequency({ Min = 2; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" 2 maal daags ", Frequency({ Min = 2; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" 2 maal per week ", Frequency({ Min = 2; Max = 2; Time = 1; Unit = "week"}) |> Some)
                (" 2 vaccinaties, als volgt: ", None)
                (" 2-3 dd ", Frequency({ Min = 2; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" 2-4 dd ", Frequency({ Min = 2; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 2-4 maal daags ", Frequency({ Min = 2; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 2-6 dd ", Frequency({ Min = 2; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" 2-6 maal daags ", Frequency({ Min = 2; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" 20 mg/ml: 6-8 dd; 40 mg/ml 3-4 dd ", Frequency({ Min = 3; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 3 dd ", Frequency({ Min = 3; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" 3 maal daags ", Frequency({ Min = 3; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" 3 maal per week ", Frequency({ Min = 3; Max = 3; Time = 1; Unit = "week"}) |> Some)
                (" 3 x per week ", Frequency({ Min = 3; Max = 3; Time = 1; Unit = "week"}) |> Some)
                (" 3-4 dd ", Frequency({ Min = 3; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 3-5 dd ", Frequency({ Min = 3; Max = 5; Time = 1; Unit = "day"}) |> Some)
                (" 3-6 dd ", Frequency({ Min = 3; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" 4 dd ", Frequency({ Min = 4; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" 4-5 dd ", Frequency({ Min = 4; Max = 5; Time = 1; Unit = "day"}) |> Some)
                (" 4-6 dd ", Frequency({ Min = 4; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" 4-6 maal daags ", Frequency({ Min = 4; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" Gedurende 4-6 uur in opklimmende hoeveelheden. ", None)
                (" Zo nodig 1-3 dd ", PRN({ Min = 1; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" afhankelijk van het analgetisch effect pleister elke 48-72 uur vervangen ", Frequency({ Min = 2; Max = 3; Time = 6; Unit = "day"}) |> Some)
                (" bolus ", None)
                (" bolus in 1 minuut ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 1-2 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 10 minuten ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 15-60 sec ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 20 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 30 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 40-80 sec ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 5 min, zo nodig elke 10 minuten herhalen tot een maximale cumulatieve dosis van 2 mg/dag ", None)
                (" bolus in 5 min. ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 5-10 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in 5-10 minuten ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" bolus in minimaal 2 minuten ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" continu infuus ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" continu infuus met een inloopsnelheid van 1-4 ml/uur. ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" continu infuus over 3 uur ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" dag 0 en dag 28, vervolgens elke 4 weken. Bij onvoldoende onderdrukking elke 3 weken. ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" eenmaal per 2 weken ", Frequency({ Min = 1; Max = 1; Time = 2; Unit = "wek"}) |> Some)
                (" elke 12 uur voor de eerste 3 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" elke 2 weken ", Frequency({ Min = 1; Max = 1; Time = 2; Unit = "week"}) |> Some)
                (" elke 30-45 minuten ", Frequency({ Min = 0; Max = 1; Time = 30; Unit = "min"}) |> Some)
                (" in 1 - 2 doses ", Frequency({ Min = 1; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" in 1 - 3 doses ", Frequency({ Min = 1; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" in 1 dosis ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "day"}) |> Some)
                (" in 1 dosis per 2 weken ", Frequency({ Min = 1; Max = 1; Time = 2; Unit = "week"}) |> Some)
                (" in 1 dosis per dagen ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "day"}) |> Some)
                (" in 10 min ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "day"}) |> Some)
                (" in 10-15 min ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "day"}) |> Some)
                (" in 15 minuten ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "day"}) |> Some)
                (" in 2 - 3 doses ", Frequency({ Min = 2; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" in 2 - 4 doses ", Frequency({ Min = 2; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" in 2 doses ", Frequency({ Min = 2; Max = 2; Time = 0; Unit = "day"}) |> Some)
                (" in 2-3 minuten ", Frequency({ Min = 1; Max = 1; Time = 1; Unit = "day"}) |> Some)
                (" in 3 - 4 doses ", Frequency({ Min = 3; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" in 3 - 5 doses ", Frequency({ Min = 3; Max = 5; Time = 1; Unit = "day"}) |> Some)
                (" in 3 - 6 doses ", Frequency({ Min = 3; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" in 3 doses ", Frequency({ Min = 3; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" in 30 min ", Frequency({ Min = 1; Max = 1; Time = 0; Unit = ""}) |> Some)
                (" in 30 minuten ", Frequency({ Min = 1; Max = 1; Time = 0; Unit = ""}) |> Some)
                (" in 4 - 24 doses ", Frequency({ Min = 2; Max = 24; Time = 1; Unit = "day"}) |> Some)
                (" in 4 - 6 doses ", Frequency({ Min = 4; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" in 4 doses ", Frequency({ Min = 4; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" in 5 min ", Frequency({ Min = 1; Max = 1; Time = 0; Unit = ""}) |> Some)
                (" in 5 min. Zo nodig nog 2 x herhalen (na 5 min) ", PRN({ Min = 1; Max = 3; Time = 0; Unit = ""}) |> Some)
                (" in 5-10 min ", Frequency({ Min = 1; Max = 1; Time = 0; Unit = ""}) |> Some)
                (" in 6 doses ", Frequency({ Min = 6; Max = 6; Time = 0; Unit = ""}) |> Some)
                (" in 60 min ", Frequency({ Min = 1; Max = 1; Time = 0; Unit = ""}) |> Some)
                (" max 2 dd ", Frequency({ Min = 0; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" maximaal 2 dd ", Frequency({ Min = 0; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" op dag 1 postpartum en op dag 3 postpartum ", Frequency({ Min = 2; Max = 2; Time = 3; Unit = "day"}) |> Some)
                (" op dag 1,4,8 en 11 ", Frequency({ Min = 4; Max = 4; Time = 11; Unit = "day"}) |> Some)
                (" op week 0, 2 en 6. Vervolgens om de 8 weken. ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" op week 0, 4 en vervolgens iedere 12 weken ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" per kant ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" per voeding ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" zo nodig ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" zo nodig 1-2 x herhalen ", PRN({ Min = 1; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig 2-4 dd ", PRN({ Min = 2; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig 3-4 dd ", PRN({ Min = 3; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig bij langdurige reizen 3 dd ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" zo nodig elke 4-6 uur ", PRN({ Min = 4; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig herhalen ", None)
                (" zo nodig herhalen na 6-8 uur ", PRN({ Min = 3; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig herhalen op geleide methomoglobine concentratie ", None)
                (" zo nodig iedere 2 uur herhalen ", PRN({ Min = 0; Max = 12; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig intra-operatief herhalen ", None)
                (" zo nodig intra-operatief herhalen als intraveneuze toediening ", None)
                (" zo nodig max 2 dd ", PRN({ Min = 0; Max = 2; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig max 3 dd ", PRN({ Min = 0; Max = 3; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig max 4 dd ", PRN({ Min = 0; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig max 5 dd ", PRN({ Min = 0; Max = 5; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig max 6 dd ", PRN({ Min = 0; Max = 6; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig max 8 dd ", PRN({ Min = 0; Max = 8; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig na 1 minuut herhalen (maximaal 4 maal herhalen) ", PRN({ Min = 0; Max = 4; Time = 1; Unit = "day"}) |> Some)
                (" zo nodig na 10 min herhalen ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" zo nodig na 5 minuten herhalen ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
                (" éénmaal per 12 weken ", Frequency({ Min = 1; Max = 1; Time = 12; Unit = "week"}) |> Some)
                (" éénmaal per 13 weken ", Frequency({ Min = 1; Max = 1; Time = 13; Unit = "week"}) |> Some)
                (" éénmalig ", Once |> Some)
                (" éénmalig (= 1 puf van 10 mg in 1 neusgat) ", Once |> Some)
                (" éénmalig (= 2 pufjes van 10 mg of 1 puf van 20 mg in 1 neusgat) ", Once |> Some)
                (" éénmalig in 1 uur ", Once |> Some)
                (" éénmalig in 10 minuten ", Once |> Some)
                (" éénmalig in 2 ml fysiologisch zout ", Once |> Some)
                (" éénmalig in 20-60 minuten ", Once |> Some)
                (" éénmalig in 5-10 minuten ", Once |> Some)
                (" éénmalig in 60 min ", Once |> Some)
                (" éénmalig in 90 minuten ", Once |> Some)
                (" éénmalig indien nodig na 30 min herhalen met 25 mg/kg/dosis ", Once |> Some)
                (" éénmalig indien nodig na 30 minuten herhalen met 25 mg/kg/dosis ", Once |> Some)
                (" éénmalig indien nodig na 30 minuten herhalen met 25-50 mg/kg/dosis ", Once |> Some)
            ]
            |> List.map (fun (s, fr) -> 
                if fr |> Option.isSome &&
                   fr.Value |> Drug.Frequency.isValid |> not then (s, None)
                else (s, fr)
            )

        let parse s =
            freqMap
            |> List.tryFind (fun (s', fr) ->
                s = s')
            |> (fun (fr) ->
                if fr.IsSome then fr.Value |> snd else None)

    module MinMaxParser =

        let parseValue = 
            (String.replace "." "") >> (String.replace "," ".") >> Double.tryParse

        let parse s =

            let ms = "\s+(\d+|\d+.\d+|\d+\s*-\s*\d+|\d+\s*-\s*\d+.\d+|\d+.\d+\s*-\s*\d+|\d+.\d+\s*-\s*\d+.\d+)\z"

            let replaceList =
                [
                    " Eénmalig op dag 1"
                ]

            let canonicalize s =
                replaceList
                |> List.fold (fun a r -> a |> String.replace r "") s
    
            let s' =
                ("#" + s
                |> canonicalize
                |> String.replace "." ""
                |> String.replace "," "."
                |> String.trim
                |> Regex.regexMatch ms).Value
                |> String.trim

            match s' |> String.splitAt "-" with
            | [|s1|] -> 
                match s1 |> parseValue with
                | Some v1 -> 
                    let mm = { Drug.MinMax.Min = Some v1; Drug.MinMax.Max = Some v1 }
                    (s, mm |> Some)
                | None -> (s, None)
            |[|s1;s2|] ->
                match (s1 |> parseValue, s2 |> parseValue) with
                | Some v1, Some v2 -> 
                    let mm = { Drug.MinMax.Min = Some v1; Drug.MinMax.Max = Some v2 }
                    (s, mm |> Some)
                | _ -> (s, None)
            | _ -> (s, None)

    module TargetParser =

        open Drug.Target


        let inline failParse o = 
            sprintf "Fail to parse %A" o
            |> failwith


        let parseQuantityUnit c (s1, s2) =
            let double s = 
                s 
                |> String.replace "." ""
                |> String.replace "," "."
                |> Double.parse
            let split = 
                (String.splitAt " ") >> Array.toList >> (List.filter String.notNullOrEmpty)
            match (s1 |> split, s2 |> split) with
            | [v1;u1], [v2;u2] ->
                createQuantity (v1 |> double) u1 |> Some ,
                createQuantity (v2 |> double) u2 |> Some  
            | [v;u], [s] ->
                if s = "None" then
                    createQuantity (v |> double) u |> Some , 
                    None
                else (s1, s2) |> failParse
            | [s], [v;u] ->
                if s = "None" then
                    None ,
                    createQuantity (v |> double) u |> Some
                else 
                    createQuantity (s |> double) u |> Some,
                    createQuantity (v |> double) u |> Some

            | _ -> None, None //(s1, s2) |> failParse
            |> c


        let parseWeight (s1, s2) =
            match s1 |> String.splitAt ":" |> Array.toList with
            | [tp; s] -> 
                match tp |> String.trim with
                | tp' when tp' = "bwght"  -> (s, s2) |> parseQuantityUnit BirthWeight
                | _ -> s1 |> failParse
            | [_] -> (s1, s2) |> parseQuantityUnit Weight  
            | _ -> (s1, s2) |> failParse      


        let parseAge (s1, s2) =
            match s1 |> String.splitAt ":" |> Array.toList with
            | [tp; s] -> 
                match tp |> String.trim with
                | tp' when tp' = "pca"  -> (s, s2) |> parseQuantityUnit PostConc        
                | tp' when tp' = "preg" -> (s, s2) |> parseQuantityUnit Pregnancy
                | _ -> s1 |> failParse
            | [_] -> (s1, s2) |> parseQuantityUnit Age  
            | _   -> (s1, s2) |> failParse      


        let parseAllWeight s =
            let split = (String.splitAt "-") >> (Array.map String.trim) >> Array.toList
            
            match s |> split with
            | [s1] ->
                if s1 |> String.contains "kg" || 
                   s1 |> String.contains "gr" ||
                   s1 |> String.contains "bwght" then (s1, s1) |> parseWeight
                else AllWeight
            | [s1;s2] ->
                if s1 |> String.contains "kg" || 
                   s1 |> String.contains "gr" ||
                   s2 |> String.contains "kg" ||
                   s2 |> String.contains "gr" then (s1, s2) |> parseWeight
                else AllWeight
            | _ -> s |> failParse


        let parseAllAge s =
            let split = (String.splitAt "-") >> (Array.map String.trim) >> Array.toList
            
            match s |> split with
            | [s1] ->
                if s1 |> String.contains "kg" || 
                   s1 |> String.contains "gr" || 
                   s1 |> String.contains "bwght" then AllAge 
                else (s1, s1) |> parseAge
            | [s1;s2] ->
                if s1 |> String.contains "kg" || 
                   s1 |> String.contains "gr" ||
                   s2 |> String.contains "kg" ||
                   s2 |> String.contains "gr" then AllAge 
                else (s1, s2) |> parseAge
            | _ -> s |> failParse


        let parseType s =
            match s |> String.replace "#" "" |> String.trim with
            | tp when tp = "aterm" -> Aterm
            | tp when tp = "neon"  -> Neonate
            | tp when tp = "prem"  -> Premature
            | tp when tp = "girl"  -> Girl
            | tp when tp = "boy"   -> Boy
            | _ -> s |> failParse


        let replaceList =
            [
                ("(zwangerschapsduur > 35 weken) a terme neonaat", "preg ; > 35 weken")
                ("geboortegewicht", "bwght:")
                ("neonaten zwangerschapsduur", "preg:")
                ("extreem prematuren zwangerschapsduur", "preg:")
                ("prematuren zwangerschapsduur", "preg:")
                ("zwangerschapsduur", "preg:")
                ("prematuren postmenstruele leeftijd", "preg:")
                ("premature en a terme neonaten postnatale leeftijd", "")
                ("extreem prematuren postconceptionele leeftijd", "pca:")
                ("prematuur postconceptionele leeftijd", "pca:")
                ("prematuren postconceptionele leeftijd", "pca:")
                ("postnatale leeftijd", "")
                ("postconceptionele leeftijd", "pca:")

                ("a terme neonaat tot", "#aterm ; ")
                ("a terme neonaat", "#aterm ; ")
                ("a terme neonaten", "#aterm ; ")
                ("neonaten", "#neon ; ")
                ("prematuren postnatale leeftijd", "#prem ; ")
                ("prematuren", "#prem ; ")
                ("menstruerende meisjes/vrouwen, vanaf circa", "#girl ; ")
                ("menstruerende meisjes/vrouwen, circa", "#girl ; ")
                ("jongens", "#boy ; ")
                ("meisjes", "#girl ; ")
                (" na menarche, ", "#girl ; ")
                ("zuigelingen en kinderen", "")

                ("1e vaccinatie:", "")
                ("2e vaccinatie:", "")
                ("3e vaccinatie:", "")
                ("4e vaccinatie:", "")

                (" tot ", " - ")
                (" en ", " ; ")
                ("<", "None -")

                ("≥ 1 kg", "1 kg - None")
                ("≥ 1,5 kg", "1,5 kg - None")
                ("≥ 2,5 kg", "2,5 kg - None")
                ("≥ 5 kg", "5 kg - None")
                ("≥ 10 kg", "10 kg - None")
                ("≥ 15 kg", "15 kg - None")
                ("≥ 20 kg", "20 kg - None")
                ("≥ 25 kg", "25 kg - None")
                ("≥ 30 kg", "30 kg - None")
                ("≥ 32,6 kg", "32,6 kg - None")
                ("≥ 33 kg", "33 kg - None")
                ("≥ 35 kg", "35 kg - None")
                ("≥ 40 kg ", "40 kg - None")
                ("≥ 41 kg ", "41 kg - None")
                ("≥ 44 kg ", "44 kg - None")
                ("≥ 45 kg ", "45 kg - None")
                ("≥ 50 kg", "50 kg - None")
                ("≥ 57 kg", "57 kg - None")
                ("≥ 70 kg", "70 kg - None")
                ("≥ 80 kg", "80 kg - None")
                ("≥ 100 kg", "100 kg - None")

                ("≥ 1250 gr", "1250 gr - None")
                ("≥ 1500 gr", "1500 gr - None")
                ("≥ 2000 gr", "2000 gr - None")

                ("≥ 7 dagen", "7 dag - None")
                ("≥ 14 dagen", "14 dag - None")

                ("≥ 4 weken", "4 week - None")
                ("≥ 30 weken", "30 week - None")
                ("≥ 32 weken", "32 week - None")
                ("≥ 34 weken", "34 week - None")
                ("≥ 35 weken", "35 week - None")
                ("≥ 36 weken", "36 week - None")
                ("≥ 37 weken", "37 week - None")

                ("≥ 1 maand", "1 maand - None")
                ("≥ 3 maanden", "3 maand - None")
                ("≥ 4 maanden", "4 maand - None")
                ("≥ 6 maanden", "6 maand - None")
                ("≥ 8 maanden", "8 maand - None")
                ("≥ 9 maanden", "9 maand - None")
                ("≥ 12 maanden", "12 maand - None")

                ("≥ 1 jaar", "1 jaar - None")
                ("≥ 2 jaar", "2 jaar - None")
                ("≥ 3 jaar", "3 jaar - None")
                ("≥ 4 jaar", "4 jaar - None")
                ("≥ 5 jaar", "5 jaar - None")
                ("≥ 6 jaar", "6 jaar - None")
                ("≥ 7 jaar", "7 jaar - None")
                ("≥ 8 jaar", "8 jaar - None")
                ("≥ 9 jaar", "9 jaar - None")
                ("≥ 10 jaar", "10 jaar - None")
                ("≥ 11 jaar", "11 jaar - None")
                ("≥ 12 jaar", "12 jaar - None")
                ("≥ 13 jaar", "13 jaar - None")
                ("≥ 14 jaar", "14 jaar - None")
                ("≥ 15 jaar", "15 jaar - None")
                ("≥ 16 jaar", "16 jaar - None")
                ("≥ 18 jaar", "18 jaar - None")

                ("dagen", "day")
                ("jaar", "year")
                ("weken", "week")
                ("maanden", "month")
                ("maand", "month")

            ]


        let parse s =

            let s' = 
                replaceList
                |> List.fold (fun a (os, ns) -> 
                    let os = os |> String.toLower
                    a |> String.replace os ns
                ) (s |> String.trim |> String.toLower)

            match s' |> String.splitAt ";" |> Array.toList with
            | [s1;s2;s3] ->
                match s1 with
                | _ when s1 |> String.contains "#" -> 
                    s1 |> parseType ,
                    s2 |> parseAllAge ,
                    s3 |> parseAllWeight

                | _ -> s1 |> failParse                 

            | [s1;s2] ->
                match s1 with
                | _ when s1 |> String.contains "#" -> 
                    s1 |> parseType ,
                    s2 |> parseAllAge ,
                    s2 |> parseAllWeight

                | _ -> 
                    AllType ,
                    s1 |> parseAllAge ,
                    s2 |> parseAllWeight                 
                    
            | [s1] ->
                match s1 with
                | _ when s1 |> String.contains "#" -> 
                    s1 |> parseType , 
                    AllAge , 
                    AllWeight

                | _ -> 
                    AllType ,
                    s1 |> parseAllAge ,
                    s1 |> parseAllWeight

            | _ -> 
                s'
                |> failParse

            |> Target



module WebSiteParser =

    open FSharp.Data
    open FSharp.Data.JsonExtensions

    open FormularyParser


    // get all medications from Kinderformularium
    let kinderFormUrl = "https://www.kinderformularium.nl/geneesmiddelen.json"


    let _medications () =
        let res = JsonValue.Load(kinderFormUrl)
        [ for v in res do
            yield Drug.createDrug (v?id.AsString()) 
                                  ""
                                  (v?generic_name.AsString()) 
                                  (v?branded_name.AsString()) ]


    let medications : unit -> Drug.Drug list = Memoization.memoize _medications


    let drugUrl = sprintf "https://www.kinderformularium.nl/geneesmiddel/%s/%s?nolayout"


    let getDoc get id gen = drugUrl id gen |> get


    let getDocSync = getDoc HtmlDocument.Load


    let getDocAsync = getDoc HtmlDocument.AsyncLoad


    let getParentFromNode n1 n2 =
        n1
        |> HtmlNode.descendants false (HtmlNode.elements >> Seq.exists ((=) n2))
        |> Seq.head

    let getParentFromDoc d n =
        d
        |> HtmlDocument.descendants false (HtmlNode.elements >> Seq.exists ((=) n))
        |> Seq.head


    let getItemType desc v d =
        d |> desc true (HtmlNode.hasAttribute "itemType" v)


    let getItemTypeFromDoc = getItemType HtmlDocument.descendants


    let getItemTypeFromNode = getItemType HtmlNode.descendants


    let getIndications d = d |> getItemTypeFromDoc "http://schema.org/MedicalIndication"


    let doseSchedule n = n |> getItemTypeFromNode "http://schema.org/DoseSchedule"


    let getItemProp v n =
        n |> HtmlNode.descendantsAndSelf true (HtmlNode.hasAttribute "itemprop" v)


    let getItemPropString v n =
        match n |> getItemProp v
                |> List.ofSeq with
        | h::_ -> h |> HtmlNode.innerText
        | _ -> ""


    let printFormulary () =
        for med in (medications () |> List.take (medications () |> List.length)) do
            printfn "\n\n========== %s ===========\n" med.Generic
            let doc = getDocSync med.Id med.Generic

            for ind in doc |> getItemTypeFromDoc "http://schema.org/MedicalIndication" do
                let name = ind |> getItemPropString "name"
                printfn "\n-- Indication: %A" name
                let ind' =
                    ind
                    |> getParentFromDoc doc
                    |> getParentFromDoc doc

                for r in ind' |> getItemProp "administrationRoute" do
                    let route = r |> getItemPropString "administrationRoute"
                    printfn "\n-- Route: %A" route
                    let r' = r |> getParentFromDoc doc

                    for dose in r' |> getItemTypeFromNode "http://schema.org/DoseSchedule" do
                        let targetPop = dose |> getItemPropString "targetPopulation"
                        let doseVals = dose |> getItemPropString "doseValue"
                        let doseUnits = dose |> getItemPropString "doseUnit"
                        let freq = dose |> getItemPropString "frequency"

                        printfn "Target Population: %s" targetPop
                        printfn "Dose: %s %s %s" doseVals doseUnits freq


    let parseDocForDoses (m: Drug.Drug) doc = 
        printfn "Parsing dose rules for: %s" m.Generic
        let atc =
            match doc 
                |> getItemTypeFromDoc "http://schema.org/MedicalCode" |> List.ofSeq with
            | h::_-> h |> getItemPropString "codeValue"
            | _ -> ""
        let getPar = getParentFromDoc doc
        { m with 
            Atc = atc
            Doses = 
                [ 
                    for i in doc |> getItemTypeFromDoc "http://schema.org/MedicalIndication" do
                        let n = i |> getItemPropString "name"
                        let i' = i |> getPar |> getPar
                        yield 
                            { 
                                Indication = n
                                Routes = 
                                    [ 
                                        for r in i' |> getItemProp "administrationRoute" do
                                            let n = r |> getItemPropString "administrationRoute"
                                            let r' = r |> getParentFromNode i' 
                                            yield 
                                                { 
                                                    Name = n
                                                    Schedules = 
                                                        [ 
                                                            for s in r' |> getItemTypeFromNode "http://schema.org/DoseSchedule" do
                                                                let tp = s |> getItemPropString "targetPopulation"
                                                                let dv = s |> getItemPropString "doseValue"
                                                                let du = s |> getItemPropString "doseUnit"
                                                                let fr = s |> getItemPropString "frequency"
                                                                yield 
                                                                    { 
                                                                        Drug.TargetText = tp |> String.trim
                                                                        Drug.Target = tp |> TargetParser.parse
                                                                        Drug.FrequencyText = fr |> String.trim
                                                                        Drug.Frequency = fr |> FrequencyParser.parse
                                                                        Drug.ValueText = dv |> String.trim
                                                                        Drug.Value = dv |> MinMaxParser.parse |> snd
                                                                        Drug.Unit = du |> String.trim
                                                                    }
                                                        ]
                                                }  
                                    ]
                        }
                ] 
        }


    let addDoses (m: Drug.Drug) = 
        async {
            let! doc = getDocAsync m.Id m.Generic
            return doc |> parseDocForDoses m
        }


    let _parseWebSite ns =
        match ns with 
        | [] ->
            medications ()
        | _ ->
            medications ()
            |> List.filter (fun m -> 
                ns |> List.exists (fun n ->
                    m.Generic |> String.startsWithCaseInsens n))
        |> List.map addDoses
        |> Async.Parallel 
        |> Async.RunSynchronously


    let cacheFormulary (ds : Drug.Drug []) =
        _parseWebSite []
        |> Json.serialize
        |> File.writeTextToFile File.cachePath


    let _getFormulary () =
        if File.cachePath |> File.exists then 
            File.cachePath
            |> File.readAllLines
            |> String.concat ""
            |> Json.deSerialize<Drug.Drug[]>
        else 
            let ds = _parseWebSite []
            ds |> cacheFormulary
            ds


    let getFormulary : unit -> Drug.Drug [] = Memoization.memoize _getFormulary

    let getRoutes () =
        getFormulary () //|> Array.length
        |> Array.collect (fun d ->
            d.Doses
            |> List.toArray
            |> Array.collect (fun dose -> 
                dose.Routes 
                |> List.map (fun r -> r.Name)
                |> List.toArray
            )
        )
        |> Array.distinct
        |> Array.sort

//
//WebSiteParser.getFormulary () //|> Array.length
//|> Array.iter (printfn "%A")



//[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
//module Target =
//
//    type Target = 
//        | Target of TargetType * TargetAge * TargetWeight
//    and TargetType = 
//        | AllType
//        | Girl
//        | Boy
//        | Neonate
//        | Aterm
//        | Premature
//    and TargetAge =
//        | AllAge
//        | Age of QuantityUnit Option * QuantityUnit Option 
//        | Pregnancy of QuantityUnit Option * QuantityUnit Option  
//        | PostConc of QuantityUnit Option * QuantityUnit Option 
//    and TargetWeight =
//        | AllWeight
//        | Weight of QuantityUnit Option * QuantityUnit Option 
//        | BirthWeight of QuantityUnit Option * QuantityUnit Option  
//    and QuantityUnit = { Quantity : float; Unit : string }
//
//    let inline failParse o = 
//        sprintf "Fail to parse %A" o
//        |> failwith
//
//    let parseQuantityUnit c (s1, s2) =
//        let double s = 
//            s 
//            |> String.replace "." ""
//            |> String.replace "," "."
//            |> Double.parse
//        let split = 
//            (String.splitAt " ") >> Array.toList >> (List.filter String.notNullOrEmpty)
//        match (s1 |> split, s2 |> split) with
//        | [v1;u1], [v2;u2] ->
//            { Quantity = v1 |> double; Unit = u1 } |> Some , 
//            { Quantity = v2 |> double; Unit = u2 } |> Some
//        | [v;u], [s] ->
//            if s = "None" then
//                { Quantity = v |> double; Unit = u } |> Some , 
//                None
//            else (s1, s2) |> failParse
//        | [s], [v;u] ->
//            if s = "None" then
//                None ,
//                { Quantity = v |> double; Unit = u } |> Some
//            else 
//                { Quantity = s |> double; Unit = u } |> Some,
//                { Quantity = v |> double; Unit = u } |> Some
//
//        | _ -> None, None //(s1, s2) |> failParse
//        |> c
//
//
//    let parseWeight (s1, s2) =
//        match s1 |> String.splitAt ":" |> Array.toList with
//        | [tp; s] -> 
//            match tp |> String.trim with
//            | tp' when tp' = "bwght"  -> (s, s2) |> parseQuantityUnit BirthWeight
//            | _ -> s1 |> failParse
//        | [_] -> (s1, s2) |> parseQuantityUnit Weight  
//        | _ -> (s1, s2) |> failParse      
//
//
//    let parseAge (s1, s2) =
//        match s1 |> String.splitAt ":" |> Array.toList with
//        | [tp; s] -> 
//            match tp |> String.trim with
//            | tp' when tp' = "pca"  -> (s, s2) |> parseQuantityUnit PostConc        
//            | tp' when tp' = "preg" -> (s, s2) |> parseQuantityUnit Pregnancy
//            | _ -> s1 |> failParse
//        | [_] -> (s1, s2) |> parseQuantityUnit Age  
//        | _ -> (s1, s2) |> failParse      
//
//
//    let parseAllWeight s =
//        let split = (String.splitAt "-") >> (Array.map String.trim) >> Array.toList
//            
//        match s |> split with
//        | [s1] ->
//            if s1 |> String.contains "kg" || 
//               s1 |> String.contains "gr" ||
//               s1 |> String.contains "bwght" then (s1, s1) |> parseWeight
//            else AllWeight
//        | [s1;s2] ->
//            if s1 |> String.contains "kg" || 
//               s1 |> String.contains "gr" ||
//               s2 |> String.contains "kg" ||
//               s2 |> String.contains "gr" then (s1, s2) |> parseWeight
//            else AllWeight
//        | _ -> s |> failParse
//
//    let parseAllAge s =
//        let split = (String.splitAt "-") >> (Array.map String.trim) >> Array.toList
//            
//        match s |> split with
//        | [s1] ->
//            if s1 |> String.contains "kg" || 
//               s1 |> String.contains "gr" || 
//               s1 |> String.contains "bwght" then AllAge 
//            else (s1, s1) |> parseAge
//        | [s1;s2] ->
//            if s1 |> String.contains "kg" || 
//               s1 |> String.contains "gr" ||
//               s2 |> String.contains "kg" ||
//               s2 |> String.contains "gr" then AllAge 
//            else (s1, s2) |> parseAge
//        | _ -> s |> failParse
//
//
//    let parseType s =
//        match s |> String.replace "#" "" |> String.trim with
//        | tp when tp = "aterm" -> Aterm
//        | tp when tp = "neon"  -> Neonate
//        | tp when tp = "prem"  -> Premature
//        | tp when tp = "girl"  -> Girl
//        | tp when tp = "boy"   -> Boy
//        | _ -> s |> failParse
//
//
//    let replaceList =
//        [
//            (" tot ", " - ")
//            (" en ", " ; ")
//            ("<", "None -")
//            ("≥ 1 kg", "1 kg - None")
//            ("≥ 1,5 kg", "1,5 kg - None")
//            ("≥ 2,5 kg", "2,5 kg - None")
//            ("≥ 5 kg", "5 kg - None")
//            ("≥ 10 kg", "10 kg - None")
//            ("≥ 15 kg", "15 kg - None")
//            ("≥ 20 kg", "20 kg - None")
//            ("≥ 25 kg", "25 kg - None")
//            ("≥ 30 kg", "30 kg - None")
//            ("≥ 32,6 kg", "32,6 kg - None")
//            ("≥ 33 kg", "33 kg - None")
//            ("≥ 35 kg", "35 kg - None")
//            ("≥ 40 kg ", "40 kg - None")
//            ("≥ 41 kg ", "41 kg - None")
//            ("≥ 44 kg ", "44 kg - None")
//            ("≥ 45 kg ", "45 kg - None")
//            ("≥ 50 kg", "50 kg - None")
//            ("≥ 57 kg", "57 kg - None")
//            ("≥ 70 kg", "70 kg - None")
//            ("≥ 80 kg", "80 kg - None")
//            ("≥ 100 kg", "100 kg - None")
//
//            ("≥ 1250 gr", "1250 gr - None")
//            ("≥ 1500 gr", "1500 gr - None")
//            ("≥ 2000 gr", "2000 gr - None")
//
//            ("≥ 7 dagen", "7 dag - None")
//            ("≥ 14 dagen", "14 dag - None")
//
//            ("≥ 4 weken", "4 week - None")
//            ("≥ 30 weken", "30 week - None")
//            ("≥ 32 weken", "32 week - None")
//            ("≥ 34 weken", "34 week - None")
//            ("≥ 35 weken", "35 week - None")
//            ("≥ 36 weken", "36 week - None")
//            ("≥ 37 weken", "37 week - None")
//
//            ("≥ 1 maand", "1 maand - None")
//            ("≥ 3 maanden", "3 maand - None")
//            ("≥ 4 maanden", "4 maand - None")
//            ("≥ 6 maanden", "6 maand - None")
//            ("≥ 8 maanden", "8 maand - None")
//            ("≥ 9 maanden", "9 maand - None")
//            ("≥ 12 maanden", "12 maand - None")
//
//            ("≥ 1 jaar", "1 jaar - None")
//            ("≥ 2 jaar", "2 jaar - None")
//            ("≥ 3 jaar", "3 jaar - None")
//            ("≥ 4 jaar", "4 jaar - None")
//            ("≥ 5 jaar", "5 jaar - None")
//            ("≥ 6 jaar", "6 jaar - None")
//            ("≥ 7 jaar", "7 jaar - None")
//            ("≥ 8 jaar", "8 jaar - None")
//            ("≥ 9 jaar", "9 jaar - None")
//            ("≥ 10 jaar", "10 jaar - None")
//            ("≥ 11 jaar", "11 jaar - None")
//            ("≥ 12 jaar", "12 jaar - None")
//            ("≥ 13 jaar", "13 jaar - None")
//            ("≥ 14 jaar", "14 jaar - None")
//            ("≥ 15 jaar", "15 jaar - None")
//            ("≥ 16 jaar", "16 jaar - None")
//            ("≥ 18 jaar", "18 jaar - None")
//
//            ("geboortegewicht", "bwght:")
//            ("neonaten zwangerschapsduur", "preg:")
//            ("extreem prematuren zwangerschapsduur", "preg:")
//            ("prematuren zwangerschapsduur", "preg:")
//            ("zwangerschapsduur", "preg:")
//            ("prematuren postmenstruele leeftijd", "preg:")
//            ("extreem prematuren postconceptionele leeftijd", "pca:")
//            ("prematuur postconceptionele leeftijd", "pca:")
//            ("prematuren postconceptionele leeftijd", "pca:")
//            ("postnatale leeftijd", "")
//            ("postconceptionele leeftijd", "pca:")
//
//            ("a terme neonaat", "#aterm ; ")
//            ("a terme neonaten", "#aterm ; ")
//            ("neonaten", "#neon ; ")
//            ("prematuren postnatale leeftijd", "#prem ; ")
//            ("prematuren", "#prem ; ")
//            ("menstruerende meisjes/vrouwen, vanaf circa", "#girl ; ")
//            ("menstruerende meisjes/vrouwen, circa", "#girl ; ")
//            ("jongens", "#boy ; ")
//            ("meisjes", "#girl ; ")
//            (" na menarche, ", "#girl ; ")
//            ("zuigelingen ; kinderen ", "")
//
//            ("1e vaccinatie:", "")
//            ("2e vaccinatie:", "")
//            ("3e vaccinatie:", "")
//            ("4e vaccinatie:", "")
//
//            ("dagen", "day")
//            ("jaar", "year")
//            ("weken", "week")
//            ("maanden", "month")
//            ("maand", "month")
//        ]
//
//
//    let parse s =
//        match s |> String.splitAt ";" |> Array.toList with
//        | [s1;s2;s3] ->
//            match s1 with
//            | _ when s1 |> String.contains "#" -> 
//                s1 |> parseType ,
//                s2 |> parseAllAge ,
//                s3 |> parseAllWeight
//
//            | _ -> s1 |> failParse                 
//
//        | [s1;s2] ->
//            match s1 with
//            | _ when s1 |> String.contains "#" -> 
//                s1 |> parseType ,
//                s2 |> parseAllAge ,
//                s2 |> parseAllWeight
//
//            | _ -> 
//                AllType ,
//                s1 |> parseAllAge ,
//                s2 |> parseAllWeight                 
//                    
//        | [s1] ->
//            match s1 with
//            | _ when s1 |> String.contains "#" -> 
//                s1 |> parseType , 
//                AllAge , 
//                AllWeight
//
//            | _ -> 
//                AllType ,
//                s1 |> parseAllAge ,
//                s1 |> parseAllWeight
//
//        | _ -> s |> failParse
//
//        |> Target
//

//WebSiteParser.getDocSync "324" "meropenem"
//|> WebSiteParser.parseDocForDoses (Drug.createDrug "324" "meropenem" "" "")

//WebSiteParser.getFormulary ()
//|> Array.filter (fun dr ->
//    dr.Doses
//    |> List.exists (fun ds ->
//        ds.Routes
//        |> List.exists (fun rt ->
//            rt.Schedules
//            |> List.exists (fun sd ->
//                match sd.Target with
//                | Drug.Target.Target(_, ta, _) ->
//                    match ta with
//                    | Drug.Target.Age(min, max) ->
//                        min |> Option.isSome && (max |> Option.isNone)
//                    | _ -> false
//            )
//        )
//    )
//) |> Array.length

//FormularyParser.FrequencyParser.freqMap
//|> List.map snd
//|> List.distinct
//|> List.iter (fun fr ->
//    match fr with
//    | Some fr' ->
//        match fr' with 
//        | Drug.Frequency.Frequency minmax | Drug.Frequency.PRN minmax ->
//            printfn "%i - %i per %i %s" minmax.Min minmax.Max minmax.Time minmax.Unit
//        | Drug.Frequency.AnteNoctum -> printfn "Antenoctum"
//        | Drug.Frequency.Once  -> printfn "Once"
//        | Drug.Frequency.Bolus -> printfn "Bolus"
//    | None -> printfn ""
//)