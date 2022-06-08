module App.PlatformSpecifics.PlatformServices

// Resources/drawable/P00_image.png additionally exists
// Resources/drawable/rsz_p00_image.png additionally exists
// Resources/drawable/SplashScreen.xml additionally exists
// Resources/values/StylesAndroid.xml additionally exists
// OverriddenMainTheme is the default theme

let init (ctx : Android.App.Activity, bundle : Android.OS.Bundle) =
    // Needs to be preserved by linker
    (new AndroidX.AppCompat.Widget.FitWindowsFrameLayout(ctx)).Dispose()

    if Xamarin.Essentials.DeviceInfo.Idiom = Xamarin.Essentials.DeviceIdiom.Phone then
        ctx.RequestedOrientation <- Android.Content.PM.ScreenOrientation.SensorPortrait
    Xamarin.Forms.Forms.SetFlags("CollectionView_Experimental")
    Xamarin.Forms.Forms.Init(ctx, bundle)
    FFImageLoading.Forms.Platform.CachedImageRenderer.Init <| System.Nullable true
    
    // Make the background color of the status bar white: https://stackoverflow.com/a/58492333
    let window = ctx.Window
    window.ClearFlags Android.Views.WindowManagerFlags.TranslucentStatus
    // add FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS flag to the window
    window.AddFlags Android.Views.WindowManagerFlags.DrawsSystemBarBackgrounds
    ctx.Window.DecorView.SystemUiVisibility <- enum <| int Android.Views.SystemUiFlags.LightStatusBar
    ctx.Window.SetStatusBarColor Android.Graphics.Color.White

    let d = Android.Content.Res.Resources.System.DisplayMetrics.Density |> float
    let statusBarHeight =
        let resourceId = ctx.Resources.GetIdentifier("status_bar_height", "dimen", "android")
        if resourceId > 0 then float (ctx.Resources.GetDimensionPixelSize resourceId) / d else 0.
    App.PlatformServices.Instance <- { new App.IPlatformServices with
        override _.ScreenDimensions =
            use rect = new Android.Graphics.Rect()
            ctx.Window.DecorView.GetWindowVisibleDisplayFrame rect
            // Use Right and Bottom instead of Width() and Height(): https://stackoverflow.com/q/7659652
            float(rect.Right) / d, float(rect.Bottom) / d - statusBarHeight
        override _.HeightDecrease = 0.
        override _.Quit() = Android.OS.Process.MyPid() |> Android.OS.Process.KillProcess
    }

    // Temporary fix for screen size problems
    ctx.Window.DecorView.SetBackgroundColor Android.Graphics.Color.White