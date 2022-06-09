// Copyright Fabulous contributors. See LICENSE.md for license.
namespace App

open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms

module App = 
    type Pedometer =
        abstract Step : IEvent<int>

    type Page =
    | Main
    | Menu
    | Alarm
    type Model = {
        Pedometer : int
        PedometerOffset: int
        Page: Page
        ScreenSize: struct(float * float)
    }

    type Msg =
    | PedometerUpdated of int
    | ScreenSizeUpdated of struct(float * float)
    | Reset
    | OpenMenu
    | CloseMenu
    | OpenAlarm
        
    let initModel = {
        Pedometer = 0
        PedometerOffset = 0
        Page = Main
        ScreenSize = PlatformServices.Instance.ScreenDimensions
    }

    let init () =
        initModel, Cmd.ofSub (fun dispatch -> DependencyService.Get<Pedometer>().Step.Add(PedometerUpdated >> dispatch))

    let update msg model =
        match msg with
        | PedometerUpdated p -> { model with Pedometer = p }, Cmd.none
        | ScreenSizeUpdated size -> { model with ScreenSize = size }, Cmd.none
        | Reset -> { model with PedometerOffset = model.Pedometer }, Cmd.none
        | OpenMenu -> { model with Page = Menu }, Cmd.none
        | CloseMenu -> { model with Page = Main }, Cmd.none
        | OpenAlarm -> { model with Page = Alarm }, Cmd.none

    // https://github.com/fsprojects/Fabulous/issues/648
    let mutable prevWH = struct(-1., -1.)
    open Views
    let view (model: Model) dispatch =
        let struct(screenWidth, screenHeight) = model.ScreenSize
        let views = Views({ Dispatch = dispatch; ScreenWidth = screenWidth; ScreenHeight = screenHeight }, 360R, 640R)
        [
            match model.Page with
            | Main | Menu ->
                views.background_rect 0xffffff
                views.background_roundRectFromTop 0xA9A290 119R 20R
                views.drawingConstrained 20R 70R 40R 20R [
                    Draw.roundRect 0xF2EFE5 3R 26R 76R 19.5<R> 0R 3R
                    Draw.roundRect 0xF2EFE5 3R 26R 84R 19.5<R> 0R 3R
                ]
                views.buttonInvisible 0R 50R 80R 60R OpenMenu
                views.text "Pedometer" 24R 0xffffff 119R (68R+24R)
                views.textCenterRect $"{model.Pedometer - model.PedometerOffset}" 55R 0x000000 0R 220R 360R 62R
                views.text "Step" 20R 0x000000 158R (299R+20R)
                views.button "Reset" 13R 0xffffff 0x645B43 ButtonStyle.Round 144R 499R 72R 21R Reset
                if model.Page = Menu then
                    views.background_escape CloseMenu
                    views.background_vRect 0xA9A290 0R 197R
                    views.drawingConstrained 145R 70R 40R 20R [
                        Draw.roundRect 0xF2EFE5 3R 152R 76R 19.5<R> 0R 3R
                        Draw.roundRect 0xF2EFE5 3R 152R 84R 19.5<R> 0R 3R
                    ]
                    views.buttonInvisible 130R 50R 80R 60R CloseMenu
                    views.background_button "Alarm" 16R 0xF2EFE5 0xB8B2A2 ButtonStyle.Rectangular 0R 119R 197R 53R OpenAlarm
            | Alarm ->
                views.text "Alarm" 24R 0x645B43 147R (68R+24R)
                views.background_roundRectFromBottom 0xA9A290 525R 20R
        ]
        |> Views._finalizeToPage (ValueOption.iter dispatch) (fun (w, h) ->
            let currWH =
                if w = 0. || h = 0.
                then PlatformServices.Instance.ScreenDimensions
                else (w, h - PlatformServices.Instance.HeightDecrease)
            if prevWH = currWH then ValueNone
            else prevWH <- currWH
                 ValueSome <| ScreenSizeUpdated currWH)

    // Note, this declaration is needed if you enable LiveUpdate
    let program =
        XamarinFormsProgram.mkProgram init update view
#if DEBUG
        |> Program.withConsoleTrace
#endif

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/tools.html#live-update for further  instructions.
    //
    // do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/Fabulous.XamarinForms/models.html#saving-application-state for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif
    static member EffectFailure (effectType: System.Type, actualControl: obj, [<System.Runtime.InteropServices.Optional>] step: string) =
        Printf.ksprintf System.Diagnostics.Debug.WriteLine "Effect %O%swas NOT applied because of an unexpected control: %O (Type:%s)"
            effectType (match step with null -> " " | x -> sprintf " at step '%s' " x) actualControl (actualControl.GetType().FullName)