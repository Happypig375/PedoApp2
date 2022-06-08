
module App.Images

type private T = struct end
let private file path =
    // Using EmbeddedResourceImageSource is best practice:
    // https://github.com/luberda-molinet/FFImageLoading/issues/696
    FFImageLoading.Forms.EmbeddedResourceImageSource(path, typeof<T>.Assembly)
    :> Xamarin.Forms.ImageSource
let ``empty.png`` = file "App.Images.empty.png"