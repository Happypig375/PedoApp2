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
    open Views
    type Action = SkiaSharp.Views.Forms.SKTouchAction
    type Pedometer =
        abstract IsSupported : bool
        abstract Step : IEvent<int>

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
        Frequency : int
    }
    type Model = {
        Pedometer : int
        PedometerOffset: int
        Page: Page
        ScreenSize: (float * float)
        Alarm: Alarm
        KnobXCoordinate: float<R>
    }

    type Message =
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
    | MoveKnob of float<R>
    | ReleaseKnob
    | StartTestNotifications
    | StopTestNotifications

    let knobXCoordinateFromFrequency frequency = float (frequency - 1) * (284R-64R)/2. + 64R
    let knobXCoordinateToFrequency x = round ((x - 64R) / ((284R-64R)/2.) + 1.) |> int
    let init () =
        let alarm =
            if Preferences.ContainsKey "alarm"
            then Preferences.Get("alarm", "") |> JsonConvert.DeserializeObject<_>
            else {
                Enabled = false
                Sunday = false
                Monday = false
                Tuesday = false
                Wednesday = false
                Thursday = false
                Friday = false
                Saturday = false
                Frequency = 1
            }
        {
            Pedometer = 0
            PedometerOffset = 0
            Page = Main
            ScreenSize = PlatformServices.Instance.ScreenDimensions
            Alarm = alarm
            KnobXCoordinate = knobXCoordinateFromFrequency alarm.Frequency
        }, Cmd.ofSub (fun dispatch ->
            let pedometer = DependencyService.Get<Pedometer>()
            if pedometer.IsSupported then
                pedometer.Step.Add(PedometerUpdated >> dispatch)
            else Application.Current.MainPage.DisplayAlert("Oh no", "Pedometer is not supported", "Ok")
                 |> ignore
        )
    let update message model =
        Xamarin.Essentials.Preferences.Set("alarm", JsonConvert.SerializeObject model.Alarm)
        let now = DateTime.Now
        let notify frequency (dayOfWeek: DayOfWeek) =
            for i in 1 .. frequency do
                NotificationCenter.Current.Show(
                    NotificationRequest(
                        Title = "Time to walk",
                        Schedule = NotificationRequestSchedule(
                            NotifyTime = now.AddDays(
                                if dayOfWeek = now.DayOfWeek then 7 else (7 + int dayOfWeek - int now.DayOfWeek) % 7
                                |> float).Date.AddHours(9 + 6 * (i - 1) |> float),
                            RepeatType = NotificationRepeat.Weekly
                        ),
                        NotificationId = int dayOfWeek + 7 * i
                    )
                ) |> ignore
        let notifyAll frequency =
            if model.Alarm.Sunday then notify frequency DayOfWeek.Sunday
            if model.Alarm.Monday then notify frequency DayOfWeek.Monday
            if model.Alarm.Tuesday then notify frequency DayOfWeek.Tuesday
            if model.Alarm.Wednesday then notify frequency DayOfWeek.Wednesday
            if model.Alarm.Thursday then notify frequency DayOfWeek.Thursday
            if model.Alarm.Friday then notify frequency DayOfWeek.Friday
            if model.Alarm.Saturday then notify frequency DayOfWeek.Saturday
        let cancel (dayOfWeek: DayOfWeek) =
            for i in 1 .. model.Alarm.Frequency do
                NotificationCenter.Current.Cancel(int dayOfWeek + 7 * i) |> ignore
        let cancelAll() = NotificationCenter.Current.CancelAll() |> ignore
        let switchDay condition dayOfWeek updatedAlarm =
            (if condition then cancel else notify model.Alarm.Frequency) dayOfWeek
            { model with Alarm = updatedAlarm }, Cmd.none
        match message with
        | PedometerUpdated p -> { model with Pedometer = p }, Cmd.none
        | ScreenSizeUpdated size -> { model with ScreenSize = size }, Cmd.none
        | Reset -> { model with PedometerOffset = model.Pedometer }, Cmd.none
        | OpenMenu -> { model with Page = Menu }, Cmd.none
        | CloseMenu -> { model with Page = Main }, Cmd.none
        | OpenAlarm -> { model with Page = Alarm }, Cmd.none
        | SwitchAlarm ->
            if model.Alarm.Enabled then cancelAll() else notifyAll model.Alarm.Frequency
            { model with Alarm = { model.Alarm with Enabled = not model.Alarm.Enabled } }, Cmd.none
        | SwitchSunday -> switchDay model.Alarm.Sunday DayOfWeek.Sunday { model.Alarm with Sunday = not model.Alarm.Sunday }
        | SwitchMonday -> switchDay model.Alarm.Monday DayOfWeek.Monday { model.Alarm with Monday = not model.Alarm.Monday }
        | SwitchTuesday -> switchDay model.Alarm.Tuesday DayOfWeek.Tuesday { model.Alarm with Tuesday = not model.Alarm.Tuesday }
        | SwitchWednesday -> switchDay model.Alarm.Wednesday DayOfWeek.Wednesday { model.Alarm with Wednesday = not model.Alarm.Wednesday }
        | SwitchThursday -> switchDay model.Alarm.Thursday DayOfWeek.Thursday { model.Alarm with Thursday = not model.Alarm.Thursday }
        | SwitchFriday -> switchDay model.Alarm.Friday DayOfWeek.Friday { model.Alarm with Friday = not model.Alarm.Friday }
        | SwitchSaturday -> switchDay model.Alarm.Saturday DayOfWeek.Saturday { model.Alarm with Saturday = not model.Alarm.Saturday }
        | MoveKnob x -> { model with KnobXCoordinate = x }, Cmd.none
        | ReleaseKnob ->
            let frequency = knobXCoordinateToFrequency model.KnobXCoordinate
            cancelAll()
            notifyAll frequency
            { model with KnobXCoordinate = knobXCoordinateFromFrequency frequency; Alarm = { model.Alarm with Frequency = frequency } }, Cmd.none
        | StartTestNotifications ->
            NotificationCenter.Current.Show(
                NotificationRequest(
                    Title = "A test notification",
                    Schedule = NotificationRequestSchedule(
                        NotifyTime = now.AddSeconds 5.,
                        RepeatType = NotificationRepeat.TimeInterval,
                        NotifyRepeatInterval = TimeSpan.FromSeconds 5.
                    ),
                    NotificationId = 999
                )
            ) |> ignore
            model, Cmd.none
        | StopTestNotifications ->
            NotificationCenter.Current.Cancel 999 |> ignore
            model, Cmd.none
    let view (model: Model) dispatch =
        let views = Views(dispatch, model.ScreenSize, 360R, 640R)
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
                views.background_rect 0xffffff
                views.text "Alarm" 94R 0x645B43 147R (68R+24R) // TODO: Font size should be 24R
                views.background_roundRectFromBottom 0xA9A290 525R 20R
                views.text "Notification" 24R 0xF2EFE5 55R (160R+24R)
                views.text "Remain you to have a walk" 14R 0xF2EFE5 55R (189R+17R) // TODO: Typo
                views.drawingConstrained 0R 0R 315R 195R [
                    Draw.group 25R 72R [Draw.path 0x645B43 3R "M19.5 2L3 14L19.5 27"]
                    Draw.roundRect (if model.Alarm.Enabled then 0xA4E3C5 else 0xF2EFE5) Draw.thicknessFill 265R 162R 46R 26R 15R
                    Draw.roundRect
                        (if model.Alarm.Enabled then 0xF3FAF7 else rgba(169, 162, 144, 0.5)) Draw.thicknessFill
                        (if model.Alarm.Enabled then 287R else 267R)
                         164R 22R 22R 11R
                ]
                views.buttonInvisible 0R 50R 75R 75R OpenMenu // TODO: Should be CloseMenu instead
                views.buttonInvisible 250R 150R 75R 0R SwitchAlarm // TODO: Set the height to 50R
                if model.Alarm.Enabled then
                    views.background_hRect (rgba(255, 255, 255, 0.2)) 280R 107R
                    views.textCenter "Repeat every" 17R 0xF2EFE5 (291R+17R)
                    let daySwitch (text: string) (left: float<R>) (on: bool) (message: Message) =
                        views.button text 20R (if on then 0xF2EFE5 else 0x848484)
                            (if on then 0x645B43 else 0xF5F3EA) ButtonStyle.Round left 330R 33R 33R message
                    daySwitch "M" 28R model.Alarm.Monday SwitchMonday
                    // TODO: Tuesday button with left X coordinate 73R. Hint: Ctrl+D to duplicate an entire line
                    daySwitch "W" 118R model.Alarm.Wednesday SwitchWednesday
                    daySwitch "T" 163R model.Alarm.Thursday SwitchThursday
                    // TODO: Friday button with left X coordinate 208R
                    daySwitch "S" 253R model.Alarm.Saturday SwitchSaturday
                    daySwitch "S" 298R model.Alarm.Sunday SwitchSunday
                    views.textCenter "Number if walk per day" 17R 0xF2EFE5 (435R+17R) // TODO: Typo
                    views.roundRect 0xF2EFE5 (70R-4.5<R>) (492R-4.5<R>) (225.5<R> + 4.5<R>*2.) (4.5<R>*2.) 4.5<R>
                    views.roundRect 0x645B43 (70R-2R) (492R-2R) (model.KnobXCoordinate - (70R-2R) + 5R) (2R * 2.) 2R
                    views.textBordered $"{knobXCoordinateToFrequency model.KnobXCoordinate}" 15R
                        0xFFFFFF 0x645B43 transparent 0R ButtonStyle.Round model.KnobXCoordinate 481R 22R 22R
                    views.touchArea false 55R 470R 275R 50R (fun x _ e ->
                        match e with
                        | Action.Moved ->
                            x - 22R/2. |> min 384R |> max 64R |> MoveKnob |> dispatch // TODO: right limit should be at 284R instead
                        | Action.Released ->
                            dispatch ReleaseKnob
                        | _ -> ()
                    )
                    views.textCenter "Some Android manufacturers like Xiaomi and Huawei disable non-system\nbackground apps so scheduled notifications are not triggered.\nThere may also be settings for running the app in background to enable.\nTry rebooting after pressing Start Testing." 10R 0xF2EFE5 530R
                    views.button "Start testing" 10R 0x848484 0xF5F3EA ButtonStyle.Rectangular 50R 590R 120R 30R StartTestNotifications
                    views.button "Stop testing" 10R 0x848484 0xF5F3EA ButtonStyle.Rectangular 200R 590R 120R 30R StopTestNotifications
        ] |> views._finalize ScreenSizeUpdated

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