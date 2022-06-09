namespace App

// Implementation is at each platform project's PlatformSpecifics folder
type IPlatformServices =
    /// In Xamarin.Forms units - same as native units, whether it be pixels or density independent units
    abstract ScreenDimensions : (float * float)
    abstract HeightDecrease : float
    abstract Quit : unit -> unit
module PlatformServices =
    let mutable Instance = Unchecked.defaultof<IPlatformServices>

module Effects =
    // ** Effects cannot be reused, make them functions instead of values! **
    let [<Literal>] _GroupName = "App"
    type BaseBasicEffect(id) = inherit Xamarin.Forms.RoutingEffect(_GroupName + "." + id)
    let baseBasic ctor = Fabulous.ViewElement.Create(ctor, (fun _ _ _ -> ()), (fun _ _ _ _ -> ()), Fabulous.AttributesBuilder 0)

    let [<Literal>] Borderless = "Borderless"
    type BorderlessEffect() = inherit BaseBasicEffect(Borderless)
    let borderless<'__> = baseBasic BorderlessEffect

    let [<Literal>] CenterPicker = "CenterPicker"
    type CenterPickerEffect() = inherit BaseBasicEffect(CenterPicker)
    let centerPicker<'__> = baseBasic CenterPickerEffect

    let [<Literal>] ButtonLeftAlign = "ButtonLeftAlign"
    type ButtonLeftAlignEffect() = inherit BaseBasicEffect(ButtonLeftAlign)
    let buttonLeftAlign<'__> = baseBasic ButtonLeftAlignEffect

    let [<Literal>] ZeroPadding = "ZeroPadding"
    type ZeroPaddingEffect() = inherit BaseBasicEffect(ZeroPadding)
    let zeroPadding<'__> = baseBasic ZeroPaddingEffect

    let [<Literal>] XF7538_AndroidEntryCenterAlign = "XF7538_AndroidEntryCenterAlign"
    type XF7538_AndroidEntryCenterAlignEffect() = inherit BaseBasicEffect(XF7538_AndroidEntryCenterAlign)
    let xf7538_AndroidEntryCenterAlign<'__> = baseBasic XF7538_AndroidEntryCenterAlignEffect