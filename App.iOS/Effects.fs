namespace PanaIoTApp.PlatformSpecifics
type Export = Xamarin.Forms.ExportEffectAttribute
type App = App.App
module Effects = App.Effects
[<assembly: Xamarin.Forms.ResolutionGroupName(Effects._GroupName)>] do ()

type Base<'T>(attached, detached, getter : Base<'T> -> 'T) =
    inherit Xamarin.Forms.Platform.iOS.PlatformEffect()
    override this.OnAttached() = getter this |> attached
    override this.OnDetached() = Option.iter ((|>) <| getter this) detached
type Container(attached, ?detached) = inherit Base<UIKit.UIView>(attached, detached, fun this -> this.Container)
type Control(attached, ?detached) = inherit Base<UIKit.UIView>(attached, detached, fun this -> this.Control)
type ArgumentedControl<'Effect when 'Effect :> Effects.BaseBasicEffect>(attached, ?detached) =
    inherit Base<'Effect * UIKit.UIView>(attached, detached,
            fun this -> this.Element.Effects |> Seq.pick (function :? 'Effect as s -> Some s | _ -> None), this.Control)
type ArgumentedContainer<'Effect when 'Effect :> Effects.BaseBasicEffect>(attached, ?detached) =
    inherit Base<'Effect * UIKit.UIView>(attached, detached,
            fun this -> this.Element.Effects |> Seq.pick (function :? 'Effect as s -> Some s | _ -> None), this.Container)

type Borderless() = inherit Control(function
    | :? UIKit.UITextField as text -> 
        text.Layer.BorderWidth <- System.nfloat 0.
        text.BorderStyle <- UIKit.UITextBorderStyle.None
    | c -> Effects.Failure.Report(typeof<Borderless>, c)
)
[<Export(typeof<Borderless>, Effects.Borderless)>] do ()

type CenterPicker() = inherit Control(function
    | :? UIKit.UITextField as pickerEntry -> pickerEntry.TextAlignment <- UIKit.UITextAlignment.Center
    | c -> Effects.Failure.Report(typeof<CenterPicker>, c)
)
[<Export(typeof<CenterPicker>, Effects.CenterPicker)>] do ()

type ButtonLeftAlign() =
    inherit Control(function
    | :? UIKit.UIButton as button -> button.HorizontalAlignment <- UIKit.UIControlContentHorizontalAlignment.Left
    | c -> Effects.Failure.Report(typeof<ButtonLeftAlign>, c)
    , function
    | :? UIKit.UIButton as button -> button.HorizontalAlignment <- UIKit.UIControlContentHorizontalAlignment.Center
    | _ -> ())
[<Export(typeof<ButtonLeftAlign>, Effects.ButtonLeftAlign)>] do ()

type ZeroPadding() = inherit Control(fun c ->
    c.LayoutMargins <- UIKit.UIEdgeInsets()
)
[<Export(typeof<ZeroPadding>, Effects.ZeroPadding)>] do ()