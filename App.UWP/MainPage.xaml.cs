using Windows.Foundation;
using Windows.UI.ViewManagement;

namespace App.UWP
{
    public sealed partial class MainPage
    {
        public MainPage()
        {
            InitializeComponent();
            LoadApplication(new global::App.App());
        }
    }
}
