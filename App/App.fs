// Copyright Fabulous contributors. See LICENSE.md for license.
namespace App

open System
open Newtonsoft.Json
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Xamarin.Essentials
open Plugin.LocalNotification

module App = 
    type Pedometer =
        abstract IsSupported : bool
        abstract Step : int IEvent

    type Page =
    | Main
    | Menu
    | Alarm
    type Alarm = {
        Enabled : bool
        Sunday : bool
        Monday : bool
        Tuesday : bool
        Wednesday : bool
        Thursday : bool
        Friday : bool
        Saturday : bool
    }
    type Model = {
        Pedometer : int
        PedometerOffset: int
        Page: Page
        ScreenSize: (float * float)
        Alarm: Alarm
    }

    type Msg =
    | PedometerUpdated of int
    | ScreenSizeUpdated of (float * float)
    | Reset
    | OpenMenu
    | CloseMenu
    | OpenAlarm
    | SwitchAlarm
    | SwitchSunday
    | SwitchMonday
    | SwitchTuesday
    | SwitchWednesday
    | SwitchThursday 
    | SwitchFriday
    | SwitchSaturday 
        
    let initModel = {
        Pedometer = 0
        PedometerOffset = 0
        Page = Main
        ScreenSize = PlatformServices.Instance.ScreenDimensions
        Alarm =
            if Preferences.ContainsKey "alarm"
            then Xamarin.Essentials.Preferences.Get("alarm", "") |> JsonConvert.DeserializeObject<_>
            else {
                Enabled = false
                Sunday = false
                Monday = false
                Tuesday = false
                Wednesday = false
                Thursday = false
                Friday = false
                Saturday = false
            }
    }

    let init () =
        initModel, Cmd.ofSub (fun dispatch ->
            let pedometer = DependencyService.Get<Pedometer>()
            if pedometer.IsSupported then
                pedometer.Step.Add(PedometerUpdated >> dispatch)
            else Xamarin.Forms.Application.Current.MainPage.DisplayAlert("Oh no", "Pedometer is not supported", "Ok")
                 |> ignore)

    let update msg model =
        Xamarin.Essentials.Preferences.Set("alarm", JsonConvert.SerializeObject model.Alarm)
        let now = DateTime.Now
        let notify (dayOfWeek: DayOfWeek) =
            NotificationCenter.Current.Show(
                NotificationRequest(
                    Title = "Time to walk",
                    Schedule = NotificationRequestSchedule(
                        NotifyTime = now.AddDays(
                            if dayOfWeek = now.DayOfWeek then 7 else (7 + int dayOfWeek - int now.DayOfWeek) % 7
                            |> float).Date,
                        RepeatType = NotificationRepeat.Weekly
                    ),
                    NotificationId = int dayOfWeek
                )
            ) |> ignore
        let cancel (dayOfWeek: DayOfWeek) = NotificationCenter.Current.Cancel(int dayOfWeek) |> ignore
        match msg with
        | PedometerUpdated p -> { model with Pedometer = p }, Cmd.none
        | ScreenSizeUpdated size -> { model with ScreenSize = size }, Cmd.none
        | Reset -> { model with PedometerOffset = model.Pedometer }, Cmd.none
        | OpenMenu -> { model with Page = Menu }, Cmd.none
        | CloseMenu -> { model with Page = Main }, Cmd.none
        | OpenAlarm -> { model with Page = Alarm }, Cmd.none
        | SwitchAlarm ->
            if model.Alarm.Enabled
            then NotificationCenter.Current.CancelAll() |> ignore
            else
                if model.Alarm.Sunday then notify DayOfWeek.Sunday
                if model.Alarm.Monday then notify DayOfWeek.Monday
                if model.Alarm.Tuesday then notify DayOfWeek.Tuesday
                if model.Alarm.Wednesday then notify DayOfWeek.Wednesday
                if model.Alarm.Thursday then notify DayOfWeek.Thursday
                if model.Alarm.Friday then notify DayOfWeek.Friday
                if model.Alarm.Saturday then notify DayOfWeek.Saturday
            { model with Alarm = { model.Alarm with Enabled = not model.Alarm.Enabled } }, Cmd.none
        | SwitchSunday ->
            if model.Alarm.Enabled then (if model.Alarm.Sunday then cancel else notify) DayOfWeek.Sunday
            { model with Alarm = { model.Alarm with Sunday = not model.Alarm.Sunday } }, Cmd.none
        | SwitchMonday ->
            if model.Alarm.Enabled then (if model.Alarm.Monday then cancel else notify) DayOfWeek.Monday
            { model with Alarm = { model.Alarm with Monday = not model.Alarm.Monday } }, Cmd.none
        | SwitchTuesday ->
            if model.Alarm.Enabled then (if model.Alarm.Tuesday then cancel else notify) DayOfWeek.Tuesday
            { model with Alarm = { model.Alarm with Tuesday = not model.Alarm.Tuesday } }, Cmd.none
        | SwitchWednesday ->
            if model.Alarm.Enabled then (if model.Alarm.Wednesday then cancel else notify) DayOfWeek.Wednesday
            { model with Alarm = { model.Alarm with Wednesday = not model.Alarm.Wednesday } }, Cmd.none
        | SwitchThursday ->
            if model.Alarm.Enabled then (if model.Alarm.Thursday then cancel else notify) DayOfWeek.Thursday
            { model with Alarm = { model.Alarm with Thursday = not model.Alarm.Thursday } }, Cmd.none
        | SwitchFriday ->
            if model.Alarm.Enabled then (if model.Alarm.Friday then cancel else notify) DayOfWeek.Friday
            { model with Alarm = { model.Alarm with Friday = not model.Alarm.Friday } }, Cmd.none
        | SwitchSaturday ->
            if model.Alarm.Enabled then (if model.Alarm.Saturday then cancel else notify) DayOfWeek.Saturday
            { model with Alarm = { model.Alarm with Saturday = not model.Alarm.Saturday } }, Cmd.none

    // https://github.com/fsprojects/Fabulous/issues/648
    let mutable prevWH = -1., -1.
    open Views
    let view (model: Model) dispatch =
        let screenWidth, screenHeight = model.ScreenSize
        let views = Views({ Dispatch = dispatch; ScreenWidth = screenWidth; ScreenHeight = screenHeight }, 360R, 640R)
        [
            match model.Page with
            | Main | Menu ->
                views.background_rect 0xffffff
                yield! views.background_roundRectFromTop 0xA9A290 119R 20R
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
                views.background_rect 0xffffff
                views.text "Alarm" 24R 0x645B43 147R (68R+24R)
                views.background_escape CloseMenu
                yield! views.background_roundRectFromBottom 0xA9A290 525R 20R
                views.text "Notification" 24R 0xF2EFE5 55R (160R+24R)
                views.text "Remain you to have a walk" 14R 0xF2EFE5 55R (189R+17R)
                views.drawingConstrained 260R 160R 55R 35R [
                    Draw.roundRect (if model.Alarm.Enabled then 0xA4E3C5 else 0xF2EFE5) Draw.thicknessFill 265R 162R 46R 26R 15R
                    Draw.roundRect
                        (if model.Alarm.Enabled then 0xF3FAF7 else rgba(169, 162, 144, 0.5)) Draw.thicknessFill
                        (if model.Alarm.Enabled then 287R else 267R)
                         164R 22R 22R 11R
                ]
                views.buttonInvisible 250R 150R 75R 50R SwitchAlarm
                views.background_hRect (rgba(255, 255, 255, 0.2)) 280R 107R
                views.text "Repeat every" 17R 0xF2EFE5 122R (291R+17R)
                let daySwitch (text: string) (left: float<R>) (on: bool) (msg: Msg) =
                    views.button text 20R (if on then 0xF2EFE5 else 0x848484)
                        (if on then 0x645B43 else 0xF5F3EA) ButtonStyle.Round left 330R 33R 33R msg
                daySwitch "M" 28R model.Alarm.Monday SwitchMonday
                daySwitch "T" 73R model.Alarm.Tuesday SwitchTuesday
                daySwitch "W" 118R model.Alarm.Wednesday SwitchWednesday
                daySwitch "T" 163R model.Alarm.Thursday SwitchThursday
                daySwitch "F" 208R model.Alarm.Friday SwitchFriday
                daySwitch "S" 253R model.Alarm.Saturday SwitchSaturday
                daySwitch "S" 298R model.Alarm.Sunday SwitchSunday
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

    static member EffectFailure (effectType: System.Type, actualControl: obj, [<System.Runtime.InteropServices.Optional>] step: string) =
        Printf.ksprintf System.Diagnostics.Debug.WriteLine "Effect %O%swas NOT applied because of an unexpected control: %O (Type:%s)"
            effectType (match step with null -> " " | x -> sprintf " at step '%s' " x) actualControl (actualControl.GetType().FullName)