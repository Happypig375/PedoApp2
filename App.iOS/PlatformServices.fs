module App.PlatformSpecifics.PlatformServices

// GetSupportedInterfaceOrientations(_, _) is overridden in AppDelegate.fs
// Resources/LaunchScreen.storyboard -> ./SplashScreen.storyboard
// Resources/SplashScreen.png additionally exists
// Resources/Default* are deleted

let supportedInterfaceOrientations =
    if Xamarin.Essentials.DeviceInfo.Idiom = Xamarin.Essentials.DeviceIdiom.Phone
    then UIKit.UIInterfaceOrientationMask.Portrait
    else UIKit.UIInterfaceOrientationMask.All

let init () =
    let iqKeyboard = Xamarin.IQKeyboardManager.SharedManager
    iqKeyboard.Enable <- true
    iqKeyboard.ShouldResignOnTouchOutside <- true
    // https://github.com/hackiftekhar/IQKeyboardManager/issues/1122#issuecomment-432897287
    iqKeyboard.LayoutIfNeededOnUpdate <- true
    
    // Make the background color of the status bar white: https://stackoverflow.com/a/58492333
    Foundation.NSOperationQueue.MainQueue.AddOperation(fun () ->
        if UIKit.UIDevice.CurrentDevice.CheckSystemVersion(13, 0) then
            let statusBar = new UIKit.UIView(UIKit.UIApplication.SharedApplication.KeyWindow.WindowScene.StatusBarManager.StatusBarFrame)
            statusBar.BackgroundColor <- UIKit.UIColor.White
            UIKit.UIApplication.SharedApplication.KeyWindow.AddSubview statusBar
        else
            let statusBar = UIKit.UIApplication.SharedApplication.ValueForKey(new Foundation.NSString "statusBar") :?> UIKit.UIView
            if statusBar.RespondsToSelector(new ObjCRuntime.Selector "setBackgroundColor:") then
                statusBar.BackgroundColor <- UIKit.UIColor.White
        UIKit.UIApplication.SharedApplication.SetStatusBarStyle(UIKit.UIStatusBarStyle.DarkContent, false)
        let window = UIKit.UIApplication.SharedApplication.KeyWindow
        let mutable vc = window.RootViewController
        while vc.PresentedViewController <> null do
            vc <- vc.PresentedViewController
        vc.SetNeedsStatusBarAppearanceUpdate()
    )

    Xamarin.Forms.Forms.SetFlags("CollectionView_Experimental")
    Xamarin.Forms.Forms.Init()
    FFImageLoading.Forms.Platform.CachedImageRenderer.Init()
    let statusBarHeight =
        let frame = UIKit.UIApplication.SharedApplication.StatusBarFrame
        min frame.Width frame.Height |> float
    App.PlatformServices.Instance <- { new App.IPlatformServices with
        override _.ScreenDimensions =
            let bounds = UIKit.UIScreen.MainScreen.Bounds
            float bounds.Width,
            float bounds.Height - statusBarHeight
        override _.HeightDecrease = statusBarHeight
        override _.Quit() = System.Threading.Thread.CurrentThread.Abort()
    }