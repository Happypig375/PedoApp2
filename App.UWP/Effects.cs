[assembly: Xamarin.Forms.ResolutionGroupName(App.Effects._GroupName)]
[assembly: Xamarin.Forms.ExportEffect(typeof(App.PlatformSpecifics.Borderless), App.Effects.Borderless)]
[assembly: Xamarin.Forms.ExportEffect(typeof(App.PlatformSpecifics.CenterPicker), App.Effects.CenterPicker)]
[assembly: Xamarin.Forms.ExportEffect(typeof(App.PlatformSpecifics.ButtonLeftAlign), App.Effects.ButtonLeftAlign)]
[assembly: Xamarin.Forms.ExportEffect(typeof(App.PlatformSpecifics.ZeroPadding), App.Effects.ZeroPadding)]

namespace App.PlatformSpecifics
{
    using System.Linq;
    using E = Windows.UI.Xaml.FrameworkElement;
    public delegate T Getter<T>(Base<T> baseEffect);
    public delegate void EffectUser<T>(T t);
    public class Base<T> : Xamarin.Forms.Platform.UWP.PlatformEffect
    {
        public Base(EffectUser<T> attached, EffectUser<T> detached, Getter<T> getter)
        { this.attached = attached; this.detached = detached; this.getter = getter; }
        readonly EffectUser<T> attached;
        readonly EffectUser<T> detached;
        readonly Getter<T> getter;
        protected override void OnAttached() => attached(getter(this));
        protected override void OnDetached() => detached?.Invoke(getter(this));
    }
    public class Container : Base<E>
    {
        public Container(EffectUser<E> attached, EffectUser<E> detached = null) : base(attached, detached, x => x.Container) { }
    }
    public class Control : Base<E>
    {
        public Control(EffectUser<E> attached, EffectUser<E> detached = null) : base(attached, detached, x => x.Control) { }
    }
    public class ArgumentedControl<Effect> : Base<System.ValueTuple<Effect, E>> where Effect : Effects.BaseBasicEffect
    {
        public ArgumentedControl(EffectUser<System.ValueTuple<Effect, E>> attached, EffectUser<System.ValueTuple<Effect, E>> detached = null)
            : base(attached, detached, x => (x.Element.Effects.OfType<Effect>().First(), x.Control)) { }
    }
    public class ArgumentedContainer<Effect> : Base<System.ValueTuple<Effect, E>> where Effect : Effects.BaseBasicEffect
    {
        public ArgumentedContainer(EffectUser<System.ValueTuple<Effect, E>> attached, EffectUser<System.ValueTuple<Effect, E>> detached = null)
            : base(attached, detached, x => (x.Element.Effects.OfType<Effect>().First(), x.Container)) { }
    }

    public class Borderless : Control
    {
        public Borderless() : base(c => {
            if (c is Windows.UI.Xaml.Controls.Control control)
                control.Margin = control.Padding = control.BorderThickness = default;
            else Effects.Failure.Report(typeof(Borderless), c);
        })
        { }
    }
    public class CenterPicker : Control
    {
        public CenterPicker() : base(c => {
            if (c is Windows.UI.Xaml.Controls.ComboBox pickerBox)
            {
                pickerBox.HorizontalContentAlignment = Windows.UI.Xaml.HorizontalAlignment.Center;
                pickerBox.Style = (Windows.UI.Xaml.Style)Windows.UI.Xaml.Application.Current.Resources["NoDropDownStyle"];
            }
            else Effects.Failure.Report(typeof(CenterPicker), c);
        })
        { }
    }
    public class ButtonLeftAlign : Control
    {
        public ButtonLeftAlign() : base(c => {
            if (c is Windows.UI.Xaml.Controls.Button button)
                button.HorizontalContentAlignment = Windows.UI.Xaml.HorizontalAlignment.Left;
            else Effects.Failure.Report(typeof(ButtonLeftAlign), c);
        }, c => {
            if (c is Windows.UI.Xaml.Controls.Button button)
                button.HorizontalContentAlignment = Windows.UI.Xaml.HorizontalAlignment.Center;
        })
        { }
    }
    public class ZeroPadding : Control
    {
        public ZeroPadding() : base(c => {
            if (c is Windows.UI.Xaml.Controls.Control control)
                control.Padding = default;
            else Effects.Failure.Report(typeof(ZeroPadding), c);
        })
        { }
    }
}