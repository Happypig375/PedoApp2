using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

namespace App.PlatformSpecifics
{

    // Modified -> Package.appxmanifest/Visual Assets
    class PlatformServices : IPlatformServices
    {
        public static void Init(Windows.ApplicationModel.Activation.IActivatedEventArgs e)
        {
            Xamarin.Forms.Forms.SetFlags("CollectionView_Experimental");
            Xamarin.Forms.Forms.Init(e);
            FFImageLoading.Forms.Platform.CachedImageRenderer.Init();

            Windows.Foundation.Size? desiredSize = new Windows.Foundation.Size(375, 812);
            if (desiredSize is Windows.Foundation.Size sz)
            {
                Windows.UI.ViewManagement.ApplicationView.GetForCurrentView()
                    .SetPreferredMinSize(new Windows.Foundation.Size(100, 100));
                Windows.UI.ViewManagement.ApplicationView.PreferredLaunchViewSize = sz;
                Windows.UI.ViewManagement.ApplicationView.PreferredLaunchWindowingMode =
                    Windows.UI.ViewManagement.ApplicationViewWindowingMode.PreferredLaunchViewSize;
                Windows.UI.ViewManagement.ApplicationView.GetForCurrentView().TryResizeView(sz);
            }
            global::App.PlatformServices.Instance = new PlatformServices();
            //Windows.UI.Xaml.Application.Current.Resources.Source = new Uri("ms-resource:///Files/_PlatformSpecifics/Styles.UWP.xaml");
        }
        public double HeightDecrease => 0;
        public System.Tuple<double, double> ScreenDimensions
        {
            get
            {
                var bounds = Windows.UI.ViewManagement.ApplicationView.GetForCurrentView().VisibleBounds;
                return Tuple.Create(bounds.Width, bounds.Height);
            }
        }
        public void Quit() => Windows.UI.Xaml.Application.Current.Exit();
    }
}