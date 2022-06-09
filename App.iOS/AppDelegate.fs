// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace App.iOS

open System
open UIKit
open Foundation
open Xamarin.Forms
open Xamarin.Forms.Platform.iOS

type PedometeriOS() =
    let pedometer = new CoreMotion.CMPedometer()
    let event = Event<int>()
    // steps from midnight
    do
        if CoreMotion.CMPedometer.IsStepCountingAvailable then
            pedometer.StartPedometerUpdates(DateTime.Today.ToNSDate(),
                Action<_, _>(fun data _ -> event.Trigger data.NumberOfSteps.Int32Value))
    interface App.App.Pedometer with
        member _.IsSupported = CoreMotion.CMPedometer.IsStepCountingAvailable
        member _.Step = event.Publish


[<Register ("AppDelegate")>]
type AppDelegate () =
    inherit FormsApplicationDelegate ()
    override _.GetSupportedInterfaceOrientations (_, _) =
        App.PlatformSpecifics.PlatformServices.supportedInterfaceOrientations
    override this.FinishedLaunching (app, options) =
        App.PlatformSpecifics.PlatformServices.init ()
        Xamarin.Forms.DependencyService.Register<PedometeriOS>()
        let appcore = new App.App()
        this.LoadApplication (appcore)
        base.FinishedLaunching(app, options)

module Main =
    [<EntryPoint>]
    let main args =
        UIApplication.Main(args, null, "AppDelegate")
        0

