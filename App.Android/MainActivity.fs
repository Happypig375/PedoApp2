// Copyright 2018 Fabulous contributors. See LICENSE.md for license.
namespace App.Android

open System

open Android.App
open Android.Content
open Android.Content.PM
open Android.Runtime
open Android.Views
open Android.Widget
open Android.OS
open Xamarin.Forms.Platform.Android
open App

type PedometerListener(onChanged) =
    inherit Java.Lang.Object()
    interface Android.Hardware.ISensorEventListener with
        member _.OnAccuracyChanged(_, _) = ()
        member _.OnSensorChanged e =
            // steps since device boot
            onChanged <| int e.Values.[0]
type PedometerAndroid() =
    let manager =  
        Context.SensorService
        |> Xamarin.Essentials.Platform.CurrentActivity.GetSystemService
        :?> Android.Hardware.SensorManager
    let sensor = manager.GetDefaultSensor Android.Hardware.SensorType.StepCounter
    let event = Event<int>()         
    do
        if sensor <> null then
            manager.RegisterListener(new PedometerListener(event.Trigger), sensor, Android.Hardware.SensorDelay.Fastest)
            |> ignore
    interface App.App.Pedometer with
        member _.IsSupported = sensor <> null
        member _.Step = event.Publish

[<UsesFeature(Name = Android.Hardware.Sensor.StringTypeStepCounter, Required = true)>]
[<UsesPermission(Android.Manifest.Permission.ActivityRecognition)>]
do ()

[<Activity (Icon = "@drawable/icon", Theme = "@style/OverriddenMainTheme", MainLauncher = true, ConfigurationChanges = (ConfigChanges.ScreenSize ||| ConfigChanges.Orientation))>]
type MainActivity() =
    inherit FormsAppCompatActivity()
    let [<Literal>] activityRecognitionRequestCode = 1483743225
    member this.RequestPedometerPermissionThenStart() =
        if AndroidX.Core.Content.ContextCompat.CheckSelfPermission(this,
            Android.Manifest.Permission.ActivityRecognition) = Permission.Denied then
            //ask for permission
            this.RequestPermissions([|Android.Manifest.Permission.ActivityRecognition|], activityRecognitionRequestCode)
        else
            Xamarin.Forms.DependencyService.Register<PedometerAndroid>()
            this.LoadApplication(App())
            Plugin.LocalNotification.NotificationCenter.NotifyNotificationTapped this.Intent |> ignore
    override this.OnNewIntent intent =
        Plugin.LocalNotification.NotificationCenter.NotifyNotificationTapped this.Intent |> ignore
        base.OnNewIntent intent
    override this.OnCreate(bundle: Bundle) =
        FormsAppCompatActivity.TabLayoutResource <- Resources.Layout.Tabbar
        FormsAppCompatActivity.ToolbarResource <- Resources.Layout.Toolbar
        base.OnCreate (bundle)
        Xamarin.Essentials.Platform.Init(this, bundle)
        App.PlatformSpecifics.PlatformServices.init(this, bundle)
        Plugin.LocalNotification.NotificationCenter.CreateNotificationChannel() |> ignore
        this.RequestPedometerPermissionThenStart()
    override this.OnRequestPermissionsResult(requestCode: int, permissions: string[], [<GeneratedEnum>] grantResults: Permission[]) =
        Xamarin.Essentials.Platform.OnRequestPermissionsResult(requestCode, permissions, grantResults)
        base.OnRequestPermissionsResult(requestCode, permissions, grantResults)
        if requestCode = activityRecognitionRequestCode then this.RequestPedometerPermissionThenStart()