// #r @"G:\source\repos\FSharp.DesigningWithTypes\FSharp.DesigningWithTypes\bin\Debug\FSharp.DesigningWithTypes.dll"

module Clock

    type T = Clock of AmPm
    and Hours = Hours of int
    and Minutes = Minutes of int
    and Seconds = Seconds of int
    and Time = (Hours * Minutes * Seconds)
    and AmPm =
        | AM of Time
        | PM of Time


    [<AutoOpen>]
    module ClockHelpers =


        let convertTime hours minutes seconds =
            let calchours h =   ((h/12), (h % 12))
            let calcminutes m = ((m/60),(m % 60))
            let calcseconds s = ((s/60),(s % 60))
            let h = calchours hours
            let m = calcminutes minutes
            let s = calcseconds seconds
            let newHours = if (snd h + fst m) = 0 
                           then 12
                           else (snd h + fst m)
            let newMinutes = snd m + fst s
            let newSeconds = snd s
            (Hours newHours, Minutes newMinutes, Seconds newSeconds)


        let create (ampm:string) h m s =
            let (hours, minutes, seconds) = convertTime h m s
            match ampm.ToLower().Trim() with
            | "am" -> Ok <| AM (hours,minutes,seconds)
            | "pm" -> Ok <| PM (hours,minutes,seconds)
            | _ -> Error <| sprintf "%s not recognized, must be AM or PM" ampm
            
        
        


