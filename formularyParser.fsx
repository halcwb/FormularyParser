
#I "./packages/ExcelProvider/lib"
#r "ExcelProvider.dll"

#I "./packages/FSharp.Data/lib/net40"
#r "FSharp.Data.dll"

#I "./packages/HtmlAgilityPack/lib/net40"
#r "HtmlAgilityPack.dll"

#I "./packages/Newtonsoft.Json/lib/net40"
#r "Newtonsoft.Json.dll"

#time

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

    let parse s = Double.Parse(s, Globalization.CultureInfo.InvariantCulture)

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


    type Schedule = 
        {
            Target : string
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



module FormularyParser =
    
    open System


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
            (" eenmaal per 2 weken ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" elke 12 uur voor de eerste 3 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" elke 2 weken ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" elke 30-45 minuten ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 1 - 2 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 1 - 3 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 1 dosis ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 1 dosis per 2 weken ", Frequency({ Min = 1; Max = 1; Time = 2; Unit = "week"}) |> Some)
            (" in 1 dosis per dagen ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 10 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 10-15 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 15 minuten ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 2 - 3 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 2 - 4 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 2 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 2-3 minuten ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 3 - 4 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 3 - 5 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 3 - 6 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 3 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 30 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 30 minuten ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 4 - 24 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 4 - 6 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 4 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 5 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 5 min. Zo nodig nog 2 x herhalen (na 5 min) ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 5-10 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 6 doses ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" in 60 min ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" max 2 dd ", Frequency({ Min = 0; Max = 2; Time = 1; Unit = "day"}) |> Some)
            (" maximaal 2 dd ", Frequency({ Min = 0; Max = 2; Time = 1; Unit = "day"}) |> Some)
            (" op dag 1 postpartum en op dag 3 postpartum ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" op dag 1,4,8 en 11 ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
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
            (" éénmalig ", Frequency({ Min = 1; Max = 1; Time = 0; Unit = ""}) |> Some)
            (" éénmalig (= 1 puf van 10 mg in 1 neusgat) ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
            (" éénmalig (= 2 pufjes van 10 mg of 1 puf van 20 mg in 1 neusgat) ", Frequency({ Min = 0; Max = 0; Time = 0; Unit = ""}) |> Some)
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

    let mapFrequency s =
        freqMap
        |> List.tryFind (fun (s', fr) ->
            s = s')
        |> (fun (fr) ->
            if fr.IsSome then fr.Value |> snd else None)

    let parseValue = 
        (String.replace "." "") >> (String.replace "," ".") >> Double.tryParse

    let mapValue s =

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
                let mm = { Drug.MinMax.Min = None; Drug.MinMax.Max = Some v1 }
                (s, mm |> Some)
            | None -> (s, None)
        |[|s1;s2|] ->
            match (s1 |> parseValue, s2 |> parseValue) with
            | Some v1, Some v2 -> 
                let mm = { Drug.MinMax.Min = Some v1; Drug.MinMax.Max = Some v2 }
                (s, mm |> Some)
            | _ -> (s, None)
        | _ -> (s, None)



module WebSiteParser =

    open FSharp.Data
    open FSharp.Data.JsonExtensions

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

    let getParent d n =
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

    let addDoses (m: Drug.Drug) = 
        async {
            let! doc = getDocAsync m.Id m.Generic
            let atc =
                match doc 
                    |> getItemTypeFromDoc "http://schema.org/MedicalCode" |> List.ofSeq with
                | h::_-> h |> getItemPropString "codeValue"
                | _ -> ""
            let getPar = getParent doc
            return 
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
                                                    let r' = r |> getPar
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
                                                                                Drug.Target = tp
                                                                                Drug.FrequencyText = fr 
                                                                                Drug.Frequency = fr |> FormularyParser.mapFrequency
                                                                                Drug.ValueText = dv
                                                                                Drug.Value = dv |> FormularyParser.mapValue |> snd
                                                                                Drug.Unit = du
                                                                            }
                                                                ]
                                                        }  
                                            ]
                                }
                        ] 
                }
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



    
//WebSiteParser.getFormulary ()
//|> Array.filter (fun d ->
//    d.Generic.Contains("Paracetamol")
//)
//|> Array.iter (printfn "%A")


