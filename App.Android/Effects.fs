namespace App.PlatformSpecifics
type Export = Xamarin.Forms.ExportEffectAttribute
type App = App.App
module Effects = App.Effects
[<assembly: Xamarin.Forms.ResolutionGroupName(Effects._GroupName)>] do ()

type Base<'T>(attached, detached, getter : Base<'T> -> 'T) =
    inherit Xamarin.Forms.Platform.Android.PlatformEffect()
    override this.OnAttached() = getter this |> attached
    override this.OnDetached() = detached |> Option.iter (getter this |> (|>))
type Container(attached, ?detached) = inherit Base<Android.Views.ViewGroup>(attached, detached, fun this -> this.Container)
type Control(attached, ?detached) = inherit Base<Android.Views.View>(attached, detached, fun this -> this.Control)
type ArgumentedControl<'Effect when 'Effect :> Effects.BaseBasicEffect>(attached, ?detached) =
    inherit Base<'Effect * Android.Views.View>(attached, detached,
            fun this -> this.Element.Effects |> Seq.pick (function :? 'Effect as s -> Some s | _ -> None), this.Control)
type ArgumentedContainer<'Effect when 'Effect :> Effects.BaseBasicEffect>(attached, ?detached) =
    inherit Base<'Effect * Android.Views.ViewGroup>(attached, detached,
            fun this -> this.Element.Effects |> Seq.pick (function :? 'Effect as s -> Some s | _ -> None), this.Container)

type Borderless() = inherit Control(fun c ->
    c.LayoutParameters <-
        new Android.Views.ViewGroup.MarginLayoutParams(c.LayoutParameters)
        |> fun p -> p.SetMargins(0, 0, 0, 0); p
    c.SetPadding(0, 0, 0, 0)
    c.Background <- null
)
[<assembly: Export(typeof<Borderless>, Effects.Borderless)>] do ()

type CenterPicker() = inherit Control(function
    | :? Android.Widget.EditText as pickerEntry ->
        pickerEntry.Gravity <- Android.Views.GravityFlags.Center
    | c -> Effects.Failure.Report(typeof<CenterPicker>, c)
)
[<assembly: Export(typeof<CenterPicker>, Effects.CenterPicker)>] do ()

type ButtonLeftAlign() =
    inherit Control(function
    | :? Android.Widget.Button as button ->
        button.Gravity <- Android.Views.GravityFlags.Left ||| Android.Views.GravityFlags.CenterVertical
    | c -> Effects.Failure.Report(typeof<ButtonLeftAlign>, c)
    , function
    | :? Android.Widget.Button as button ->
        button.Gravity <- Android.Views.GravityFlags.Center
    | _ -> ()
)
[<assembly: Export(typeof<ButtonLeftAlign>, Effects.ButtonLeftAlign)>] do ()

type ZeroPadding() = inherit Control(fun c -> c.SetPadding(0, 0, 0, 0))
[<assembly: Export(typeof<ZeroPadding>, Effects.ZeroPadding)>] do ()

type XF7538_AndroidEntryCenterAlign() =
    inherit Control(
        function
        | :? Android.Widget.EditText as entry -> entry.Gravity <- Android.Views.GravityFlags.Center
        | c -> Effects.Failure.Report(typeof<XF7538_AndroidEntryCenterAlign>, c)
      , function
        | :? Android.Widget.EditText as entry -> entry.Gravity <- Android.Views.GravityFlags.Start | _ -> ())
[<assembly: Export(typeof<XF7538_AndroidEntryCenterAlign>, Effects.XF7538_AndroidEntryCenterAlign)>] do ()
                         
// https://github.com/xamarin/Xamarin.Forms/issues/2444#issuecomment-511296066
#nowarn "44" // Xamarin.Forms.Forms.Context is deprecated
type XF8262_AndroidLabelCrash_Label =
    inherit Xamarin.Forms.Platform.Android.FormsTextView
    new(context) = { inherit Xamarin.Forms.Platform.Android.FormsTextView(context) }
    // Android 4.3 - to avoid this crash : Unable to activate instance of type Xamarin.Forms.Platform.Android.FormsTextView
    new(_, _) = { inherit Xamarin.Forms.Platform.Android.FormsTextView(Xamarin.Forms.Forms.Context) }
type XF8262_AndroidLabelCrash(context) =
    inherit Xamarin.Forms.Platform.Android.LabelRenderer(context)
    override _.CreateNativeControl() = new XF8262_AndroidLabelCrash_Label(context) :> _
[<assembly: Xamarin.Forms.ExportRenderer(typeof<Xamarin.Forms.Label>, typeof<XF8262_AndroidLabelCrash>)>] do ()