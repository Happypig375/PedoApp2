namespace App
open System
open SkiaSharp
open Xamarin.Forms
open Fabulous
open Fabulous.XamarinForms
open Fabulous.XamarinForms.FFImageLoading
open Fabulous.XamarinForms.SkiaSharp
module Views =
 type [<Struct>] GenericOneWithTypeOf =
     static member ($) (_: GenericOneWithTypeOf, _: sbyte<'M>) = LanguagePrimitives.SByteWithMeasure<'M> 1y
     static member ($) (_: GenericOneWithTypeOf, _: int16<'M>) = LanguagePrimitives.Int16WithMeasure<'M> 1s
     static member ($) (_: GenericOneWithTypeOf, _: int<'M>) = LanguagePrimitives.Int32WithMeasure<'M> 1
     static member ($) (_: GenericOneWithTypeOf, _: int64<'M>) = LanguagePrimitives.Int64WithMeasure<'M> 1L
     static member ($) (_: GenericOneWithTypeOf, _: float32<'M>) = LanguagePrimitives.Float32WithMeasure<'M> 1.f
     static member ($) (_: GenericOneWithTypeOf, _: float<'M>) = LanguagePrimitives.FloatWithMeasure<'M> 1.
     static member ($) (_: GenericOneWithTypeOf, _: decimal<'M>) = LanguagePrimitives.DecimalWithMeasure<'M> 1m
 let inline GenericOneWithTypeOf x = GenericOneWithTypeOf() $ x
 let inline (=~) (a:^a) (b:^a) = abs (a / GenericOneWithTypeOf a - b / GenericOneWithTypeOf b |> float) <= 0.0001
 let rgba(red, green, blue, alpha) = (alpha * 255. |> int |> (+) 1 <<< 24) + (red <<< 16) + (green <<< 8) + blue
 let transparent = 0x01000000
 let gray = 0x808080
 let hexToUint32 hex = if hex = transparent then 0 elif hex &&& 0xff000000 = 0 then hex + 0xff000000 else hex
                       |> uint32
 let colorHex hex = hexToUint32 hex |> Color.FromUint
 type [<Measure>] R
 module NumericLiteralR =
     let inline FromZero() = 0.<R>
     let inline FromOne() = 1.<R>
     let inline FromInt32 x = float x |> LanguagePrimitives.FloatWithMeasure<R>
 type [<Struct>] ButtonStyle = Rectangular | RectangularBold | RectangularLeft | Round | RoundBold | RoundRadius of float<R>
 type [<Struct>] LayoutOrientation = Vertical | Horizontal
 type ListStyle =
 | HorizontalLinear | HorizontalLinearHSpacing of hSpacing:float<R>
 | VerticalLinear | VerticalLinearVSpacing of vSpacing:float<R>
 | HorizontalGrid of rowCount:int | HorizontalGridHSpacing of rowCount:int * hSpacing:float<R>
 | HorizontalGridVSpacing of rowCount:int * vSpacing:float<R> | HorizontalGridHVSpacing of rowCount:int * hSpacing:float<R> * vSpacing:float<R>
 | VerticalGrid of columnCount:int | VerticalGridHSpacing of columnCount:int * hSpacing:float<R>
 | VerticalGridVSpacing of rowCount:int * vSpacing:float<R> | VerticalGridHVSpacing of rowCount:int * hSpacing:float<R> * vSpacing:float<R>
 module ListStyle =
     let toItemsLayout ratio = function
     | HorizontalLinear -> LinearItemsLayout ItemsLayoutOrientation.Horizontal :> ItemsLayout
     | HorizontalLinearHSpacing hSpacing -> LinearItemsLayout (ItemsLayoutOrientation.Horizontal, ItemSpacing = hSpacing * ratio) :> _
     | VerticalLinear -> LinearItemsLayout ItemsLayoutOrientation.Vertical :> _
     | VerticalLinearVSpacing vSpacing -> LinearItemsLayout (ItemsLayoutOrientation.Vertical, ItemSpacing = vSpacing * ratio) :> _
     | HorizontalGrid rowCount -> GridItemsLayout(rowCount, ItemsLayoutOrientation.Horizontal) :> _
     | HorizontalGridHSpacing (rowCount, hSpacing) -> GridItemsLayout(rowCount, ItemsLayoutOrientation.Horizontal, HorizontalItemSpacing = hSpacing * ratio) :> _
     | HorizontalGridVSpacing (rowCount, vSpacing) -> GridItemsLayout(rowCount, ItemsLayoutOrientation.Horizontal, VerticalItemSpacing = vSpacing * ratio) :> _
     | HorizontalGridHVSpacing (rowCount, hSpacing, vSpacing) ->
         GridItemsLayout(rowCount, ItemsLayoutOrientation.Horizontal, HorizontalItemSpacing = hSpacing * ratio, VerticalItemSpacing = vSpacing * ratio) :> _
     | VerticalGrid columnCount -> GridItemsLayout(columnCount, ItemsLayoutOrientation.Vertical) :> _
     | VerticalGridHSpacing (rowCount, hSpacing) -> GridItemsLayout(rowCount, ItemsLayoutOrientation.Vertical, HorizontalItemSpacing = hSpacing * ratio) :> _
     | VerticalGridVSpacing (rowCount, vSpacing) -> GridItemsLayout(rowCount, ItemsLayoutOrientation.Vertical, VerticalItemSpacing = vSpacing * ratio) :> _
     | VerticalGridHVSpacing (rowCount, hSpacing, vSpacing) ->
         GridItemsLayout(rowCount, ItemsLayoutOrientation.Vertical, HorizontalItemSpacing = hSpacing * ratio, VerticalItemSpacing = vSpacing * ratio) :> _
 let applyBoundsFrom (source:ViewElement) target =
     match source.TryGetAttributeKeyed ViewAttributes.LayoutBoundsAttribKey with
     | ValueSome rect -> layoutBounds rect target | ValueNone -> target
 let applyReplace (attributeKey:AttributeKey<'T>) valueReplacer (view:ViewElement) =
    for i in 0 .. view.AttributesKeyed.Length - 1 do
        let kv = view.AttributesKeyed.[i]
        if kv.Key = attributeKey.KeyValue
        then view.AttributesKeyed.[i] <- System.Collections.Generic.KeyValuePair
                (kv.Key, kv.Value :?> 'T |> valueReplacer |> box<'T>)
    view
 let applyBoundsMap = applyReplace ViewAttributes.LayoutBoundsAttribKey
 let applyEffect effect = applyReplace ViewAttributes.EffectsAttribKey (Array.append [|effect|])
 let applyOpacity value = opacity value
 let applyButtonLeftAlignment view = effects [Effects.buttonLeftAlign] view
 let applyTextBold view = fontAttributes FontAttributes.Bold view
 type Keyboard = Xamarin.Forms.Keyboard
 let passwordKeyboardFlags = enum<KeyboardFlags> -375
 type Xamarin.Forms.Keyboard with
     static member Password = Keyboard.Create passwordKeyboardFlags
 type [<NoEquality; NoComparison>] DrawElement =
 | DrawElement of (SKCanvas -> SKPaint -> unit)
 | DrawTransform of SKMatrix * DrawElement
 | DrawTransformMulti of SKMatrix * DrawElement seq
 module Draw =
     let thicknessFill = -1R
     let private skMeasure = float32<float<R>>
     let private assign color thickness (paint:SKPaint) =
         paint.StrokeWidth <- skMeasure thickness
         paint.Color <- hexToUint32 color |> SKColor.op_Implicit
         paint.Style <- if thickness = thicknessFill then SKPaintStyle.Fill else SKPaintStyle.Stroke
         paint.IsAntialias <- true
         paint
     let translate dx dy drawing =
         DrawTransform(SKMatrix.MakeTranslation(skMeasure dx, skMeasure dy), drawing)
     let group dx dy drawings =
         DrawTransformMulti(SKMatrix.MakeTranslation(skMeasure dx, skMeasure dy), drawings)
     let circle color thickness left top cornderRadius =
         DrawElement(fun canvas paint ->
            canvas.DrawCircle(skMeasure (left+cornderRadius), skMeasure (top+cornderRadius), skMeasure cornderRadius, assign color thickness paint))
     let rect color thickness left top width height =
         DrawElement(fun canvas paint ->
            canvas.DrawRect(skMeasure left, skMeasure top, skMeasure width, skMeasure height, assign color thickness paint))
     let roundRect color thickness left top width height cornerRadius =
         DrawElement(fun canvas paint ->
            canvas.DrawRoundRect(skMeasure left, skMeasure top, skMeasure width, skMeasure height, skMeasure cornerRadius, skMeasure cornerRadius, assign color thickness paint))
     let polygon color thickness points =
         DrawElement(fun canvas paint ->
            use path = new SKPath()
            path.AddPoly(points)
            canvas.DrawPath(path, assign color thickness paint))
     let path color thickness path =
         DrawElement(fun canvas paint ->
            use path = SKPath.ParseSvgPathData path in canvas.DrawPath(path, assign color thickness paint))
 type [<Struct>] SelectedPickerID<'ID when 'ID : equality> = SelectedPickerID of 'ID | SelectedNoPickers
 type [<NoComparison; NoEquality>] Picker<'ID, 'UpdateDropdown when 'ID : equality> =
    Picker of 'ID * baseButton:('UpdateDropdown -> ViewElement) * dropdown:('UpdateDropdown -> ViewElement list)
open Views
/// Views from SVG data
type Views<'Message>(dispatch : 'Message -> unit, screenSize: float * float, totalWidth : float<R>, totalHeight : float<R>) =
 // https://github.com/fsprojects/Fabulous/issues/648
 static let mutable _prevWidthHeight = -1., -1.
 let screenWidthInDevicePixels, screenHeightInDevicePixels = screenSize
 /// Screen units to layout units ratio 
 let ratio = min (screenWidthInDevicePixels / totalWidth) (screenHeightInDevicePixels / totalHeight)
 // The iPhone 6S sometimes makes views with width/height less than 0.5 device units disappear
 let guardEpsilons n = if 0. < n && n < 0.5 then 0.5 else n
 let boundsXOffset, boundsYOffset =
    (screenWidthInDevicePixels - totalWidth * ratio) / 2., (screenHeightInDevicePixels - totalHeight * ratio) / 2.
 let background_bounds left top width height =
     let pos posComp offset =
         if posComp =~ 0R then 0. else posComp * ratio + offset
     let size posComp sizeComp offset total =
         sizeComp * ratio
         + if posComp =~ 0R then offset else 0.
         + if posComp + sizeComp =~ total then offset else 0.
         |> guardEpsilons
     Rectangle (
         pos left boundsXOffset,
         pos top boundsYOffset,
         size left width boundsXOffset totalWidth,
         size top height boundsYOffset totalHeight)
     |> layoutBounds
 let background_hBounds top height = background_bounds 0R top totalWidth height
 let background_vBounds left width = background_bounds left 0R width totalHeight
 let applyBoundsWHInNativeUnits left top width height =
     let rect = Rectangle(left * ratio + boundsXOffset, top * ratio + boundsYOffset, guardEpsilons width, guardEpsilons height)
     layoutBounds rect
 let applyBounds left top width height = applyBoundsWHInNativeUnits left top (width * ratio) (height * ratio)
 member _.applyBoundsOn left top width height view = applyBounds left top width height view
 member _.applyRounded radius (view:ViewElement) =
     View.Frame(content = view, padding = Thickness 0., isClippedToBounds = true, hasShadow = false,
                cornerRadius = radius * ratio, backgroundColor = Color.Transparent)
     |> applyBoundsFrom view
 // https://github.com/xamarin/Xamarin.Forms/issues/1695 ...
 /// Doesn't support transparency on backColor!!!
 member t.applyRoundedBorder_NoBackColorTransparency radius backColor borderColor borderWidth view =
     view |> t.applyRounded (radius - borderWidth) |> backgroundColor (colorHex backColor) |> 
     t.applyRounded radius |> backgroundColor (colorHex borderColor) |> padding (borderWidth * ratio |> Thickness)
     |> applyBoundsFrom view
 //// Doesn't work properly...
 ///// Doesn't support clipping the containing element to frame bounds!!!
 //let applyRoundedBorder_NoClip left top width height radius backColor borderColor borderWidth view =
 //    [
 //        View.Button(backgroundColor = colorHex backColor, cornerRadius = int (radius * ratio),
 //            borderWidth = borderWidth * ratio, borderColor = colorHex borderColor
 //          , effects = [Effects.zeroPadding()]) |> applyBounds left top width height
 //        view
 //    ]
 member _.info_totalWidth = totalWidth
 member _.info_totalHeight = totalHeight
 member _.info_isLandscape = screenWidthInDevicePixels > screenHeightInDevicePixels
 member _._finalize updateScreenSize views =
     Seq.toList views
     |> fun l -> View.AbsoluteLayout l
     |> fun a -> View.ContentPage(a, sizeAllocated = fun (width, height) ->
            let currentWidthHeight =
                if width = 0. || height = 0.
                then PlatformServices.Instance.ScreenDimensions
                else (width, height - PlatformServices.Instance.HeightDecrease)
            if _prevWidthHeight = currentWidthHeight then ValueNone
            else _prevWidthHeight <- currentWidthHeight
                 ValueSome <| updateScreenSize currentWidthHeight
            |> ValueOption.iter dispatch
      )
 member _.background_hRect hex top height = View.BoxView(backgroundColor = colorHex hex) |> background_hBounds top height
 member _.background_vRect hex left width = View.BoxView(backgroundColor = colorHex hex) |> background_vBounds left width
 member _.background_oRect hex left top width height = View.BoxView(backgroundColor = colorHex hex) |> background_bounds left top width height
 member t.background_roundRectFromTop color height cornerRadius = t.merge [
    View.BoxView(backgroundColor = colorHex color, cornerRadius = CornerRadius (cornerRadius * ratio)) |> background_hBounds 0R height
    View.BoxView(backgroundColor = colorHex color) |> background_hBounds 0R cornerRadius
 ]
 member t.background_roundRectFromBottom color height cornerRadius = t.merge [
    View.BoxView(backgroundColor = colorHex color, cornerRadius = CornerRadius (cornerRadius * ratio)) |> background_hBounds (totalHeight - height) height
    View.BoxView(backgroundColor = colorHex color) |> background_hBounds (totalHeight - cornerRadius) cornerRadius
 ]
 member t.background_rect color = t.background_oRect color 0R 0R totalWidth totalHeight
 member t.background_hLine color thickness centerY = t.background_hRect color (centerY - thickness / 2.) thickness
 member t.background_hLineFromLeft color thickness right centerY = t.background_oRect color 0R (centerY - thickness / 2.) right thickness
 member t.background_hLineFromRight color thickness left centerY = t.background_oRect color left (centerY - thickness / 2.) (totalWidth-left) thickness
 member _.background_hImage (source:ImageSource) top height = View.Image(Image.ImageSource source) |> background_hBounds top height
 member _.background_button text fontSize textColor backColor style left top width height message =
     View.Button(text, fun () -> dispatch message
       , textColor = colorHex textColor, fontSize = FontSize.Size (fontSize * ratio), padding = Thickness 0.,
         backgroundColor = colorHex backColor, borderWidth = 0., borderColor = Color.Transparent,
         fontAttributes = match style with
                          | Round | RoundRadius _ | Rectangular | RectangularLeft -> FontAttributes.None
                          | RoundBold | RectangularBold -> FontAttributes.Bold
       , cornerRadius = match style with
                        | Round | RoundBold -> min width height / 2. * ratio |> int
                        | RoundRadius r -> r * ratio |> int
                        | Rectangular | RectangularBold | RectangularLeft -> 0
       , effects = [Effects.zeroPadding])
       |> background_bounds left top width height
 member _.background_escape message = // view.g. to close current dropdown
     View.ContentView(gestureRecognizers = [
         View.TapGestureRecognizer(fun () -> dispatch message)
     ]) |> background_bounds 0R 0R totalWidth totalHeight
 
 member _.group dx dy = List.map (applyBoundsMap (fun bounds -> Rectangle(bounds.X + dx * ratio, bounds.Y + dy * ratio, bounds.Width, bounds.Height)))
 member _.text text fontSize color left baselineY =
     View.Label(text, textColor = colorHex color, fontSize = FontSize.Size (fontSize * ratio))
     |> applyBoundsWHInNativeUnits left (baselineY - fontSize) AbsoluteLayout.AutoSize AbsoluteLayout.AutoSize
 member _.textCenterSpan text fontSize color left baselineY right =
     View.Label(text, horizontalTextAlignment = TextAlignment.Center, textColor = colorHex color, fontSize = FontSize.Size (fontSize * ratio))
     |> applyBoundsWHInNativeUnits left (baselineY - fontSize) ((right-left)*ratio) AbsoluteLayout.AutoSize
 member _.textRightSpan text fontSize color left baselineY right =
     View.Label(text, horizontalTextAlignment = TextAlignment.End, textColor = colorHex color, fontSize = FontSize.Size (fontSize * ratio))
     |> applyBoundsWHInNativeUnits left (baselineY - fontSize) ((right-left)*ratio) AbsoluteLayout.AutoSize
 member _.textCenterRect text fontSize color left top width height =
     View.Label(text, horizontalTextAlignment = TextAlignment.Center, verticalTextAlignment = TextAlignment.Center, textColor = colorHex color,
         fontSize = FontSize.Size (fontSize * ratio)) |> applyBounds left top width height
 member _.textCenterMargin text fontSize color hMargin baselineY =
     View.Label(text, horizontalTextAlignment = TextAlignment.Center, textColor = colorHex color, fontSize = FontSize.Size (fontSize * ratio))
     |> applyBoundsWHInNativeUnits hMargin (baselineY - fontSize) ((totalWidth - hMargin * 2.) * ratio) AbsoluteLayout.AutoSize
 member t.textCenter text fontSize color baselineY = t.textCenterMargin text fontSize color 0R baselineY
 member t.textScroll text fontSize color left top width height = View.ScrollView(t.textCenter text fontSize color 0R) |> applyBounds left top width height
 member _.textEdit keyboard text fontSize textColor placeholder placeholderColor left baselineY right textChanged =
     let keyboard, isPassword =
         match keyboard:Keyboard with
         | :? Internals.CustomKeyboard as custom when custom.Flags = passwordKeyboardFlags -> Keyboard.Default, true
         | keyboard -> keyboard, false
     View.Entry(text, fontSize = FontSize.Size (fontSize * ratio), textColor = colorHex textColor, keyboard = keyboard, 
         placeholder = placeholder, placeholderColor = colorHex placeholderColor, backgroundColor = colorHex transparent,
         isPassword = isPassword, textChanged = fun args -> args.NewTextValue |> textChanged |> dispatch
       , effects = [Effects.borderless]
     ) |> applyBounds (left - 1R) (baselineY - fontSize - 4R) (right - left + 1R) (fontSize + 11R)
 member t.textEditCenter keyboard text fontSize textColor placeholder placeholderColor left top width height textChanged =
     t.textEdit keyboard text fontSize textColor placeholder placeholderColor 0R 0R 0R textChanged
     |> horizontalTextAlignment TextAlignment.Center
     |> verticalTextAlignment TextAlignment.Center
     |> applyEffect Effects.xf7538_AndroidEntryCenterAlign
     |> applyBounds left top width height
 member _.rect color left top width height = View.BoxView(backgroundColor = colorHex color) |> applyBounds left top width height
 member _.roundRect color left top width height cornerRadius = View.BoxView(backgroundColor = colorHex color, cornerRadius = CornerRadius (cornerRadius * ratio)) |> applyBounds (left) top width height
 member _.shadow color left top width height =
     let shadowThickness = 10R
     View.Frame(hasShadow = true, backgroundColor = colorHex color, margin = Thickness (shadowThickness * ratio))
     |> applyBounds (left - shadowThickness) (top - shadowThickness) (width + shadowThickness * 2.) (height + shadowThickness * 2.)
 member t.roundShadow color left top width height cornderRadius = t.shadow color left top width height |> frameCornerRadius (cornderRadius * ratio)
 /// x2 in svg = width here
 member _.hLine hex thickness left centerY width = View.BoxView(backgroundColor = colorHex hex) |> applyBounds left (centerY-thickness / 2.) width thickness 
 // Use loadingPlaceholder to counter previous image still being present after Fabulous reuses the image view: empty.png is a 1×1 image containing one transparent pixel
 member _.image left top width height file =
     View.CachedImage(source = Image.ImageSource file, aspect = Aspect.AspectFit, loadingPlaceholder = Image.ImageSource Images.``empty.png``) |> applyBounds left top width height
 member t.imageAround centerX centerY width height = t.image (centerX - width / 2.) (centerY - height / 2.) width height
 member _.imageFill left top width height (file:ImageSource) =
     View.CachedImage(source = Image.ImageSource file, aspect = Aspect.AspectFill, loadingPlaceholder = Image.ImageSource Images.``empty.png``) |> applyBounds left top width height
 member _.buttonInvisible left top width height message = 
     View.Button("", fun () -> dispatch message
       , padding = Thickness 0., backgroundColor = Color.Transparent, borderColor = Color.Transparent,
       effects = [Effects.zeroPadding])
       |> applyBounds left top width height
 member _.buttonInvisibleMultiPress pressCount left top width height message = 
     View.ContentView(gestureRecognizers = [View.TapGestureRecognizer(numberOfTapsRequired = pressCount, command = fun () -> dispatch message)])
     |> applyBounds left top width height
 member _.buttonMultiMsg text fontSize textColor backColor style left top width height msgs =
     View.Button(text, fun () -> Seq.iter dispatch msgs
       , textColor = colorHex textColor, fontSize = FontSize.Size (fontSize * ratio), padding = Thickness 0.,
         backgroundColor = colorHex backColor, borderWidth = 0., borderColor = Color.Transparent,
         fontAttributes = match style with
                          | Round | RoundRadius _ | Rectangular | RectangularLeft -> FontAttributes.None
                          | RoundBold | RectangularBold -> FontAttributes.Bold
       , cornerRadius = match style with
                        | Round | RoundBold -> min width height / 2. * ratio |> int
                        | RoundRadius r -> r * ratio |> int
                        | Rectangular | RectangularBold | RectangularLeft -> 0
       , effects = [Effects.zeroPadding])
       |> applyBounds left top width height
 member t.button text fontSize textColor backColor style left top width height message =
     t.buttonMultiMsg text fontSize textColor backColor style left top width height [message]
 member t.buttonBordered text fontSize textColor backColor borderColor borderThickness style left top width height message =
     t.button text fontSize textColor backColor style left top width height message
     |> ViewElementExtensions.borderColor (colorHex borderColor) |> borderWidth (borderThickness * ratio)
 member t.textBordered text fontSize textColor backColor borderColor borderThickness style left top width height =
     t.buttonBordered text fontSize textColor backColor
         borderColor borderThickness style left top width height Unchecked.defaultof<_>
     |> applyReplace ViewAttributes.CommandAttribKey (fun _ () -> ())
     |> isTabStop false
 member t.imageButton left top width height message (padding:float<_>) (file:ImageSource) = [
     t.image (left+padding) (top+padding) (width-padding*2.) (height-padding*2.) file
     t.buttonInvisible left top width height message
 ]
 member t.circle color left top cornderRadius = t.roundRect color left top (cornderRadius*2.) (cornderRadius*2.) cornderRadius
 member t.circleBordered backColor borderColor borderThickness left top (cornerRadius:float<_>) =
     t.textBordered "" 0R transparent backColor borderColor borderThickness Round left top (cornerRadius*2.) (cornerRadius*2.)
 member _.activityIndicator color left top width height =
     View.ActivityIndicator(true, color = colorHex color) |> applyBounds left top width height
 member private _.listMapTemplate template templateLeft templateTop templateRight templateBottom templateArg =
     template templateArg
     |> Option.map (fun views ->
         View.AbsoluteLayout(views |> Seq.map (applyBoundsMap(fun rect ->
                 Rectangle(rect.Left - boundsXOffset - templateLeft * ratio,
                             rect.Top - boundsYOffset - templateTop * ratio,
                             rect.Width, rect.Height))) |> List.ofSeq,
             width = (templateRight - templateLeft) * ratio,
             height = (templateBottom - templateTop) * ratio))
 member private _.listTemplate listStyle left top width height scrollTo updateScrollTo items =
     ignore (updateScrollTo : 'b -> 'a)
     ignore (scrollTo : 'b)
     View.CollectionView(items,
         itemsLayout = ListStyle.toItemsLayout ratio listStyle,
         itemSizingStrategy = ItemSizingStrategy.MeasureFirstItem
         // // Scrolling memorization for UWP causes crashes, unfortunately:
         // // https://github.com/xamarin/Xamarin.Forms/issues/8508
         // , scrolled = fun view -> view.FirstVisibleItemIndex |> updateScrollTo |> dispatch
         // , created = fun c ->
         //     if Xamarin.Essentials.DeviceInfo.Platform <> Xamarin.Essentials.DevicePlatform.UWP
         //     then c.ScrollTo(scrollTo, -1, ScrollToPosition.MakeVisible, false)
     ) |> applyBounds left top width height
 /// Temporary implementation of list using ScrollView instead of CollectionView,
 /// used to evade https://github.com/fsprojects/Fabulous/issues/609
 member t.listGrouped_ listType (groupHeaderTemplateLeft, groupHeaderTemplateTop,
                                     groupHeaderTemplateRight, groupHeaderTemplateBottom, groupHeaderTemplate)
         (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
         left top width height scrollTo updateScrollTo source =
     View.ScrollView (
         View.AbsoluteLayout (
             dependsOn
                 (source, groupHeaderTemplateLeft, groupHeaderTemplateTop, groupHeaderTemplateRight, groupHeaderTemplateBottom, groupHeaderTemplate,
                  itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
                 <| fun _ _ ->
             Seq.fold (fun (acc, leftOrTop (* left of top of group header *)) (groupHeader, items) ->
                 let groupHeader =
                     t.listMapTemplate groupHeaderTemplate groupHeaderTemplateLeft
                         groupHeaderTemplateTop groupHeaderTemplateRight groupHeaderTemplateBottom groupHeader
                     |> Option.map (
                        applyBounds
                            (-boundsXOffset/ratio + match listType with Horizontal -> leftOrTop | Vertical -> 0R)
                            (-boundsYOffset/ratio + match listType with Vertical -> leftOrTop | Horizontal -> 0R)
                            (groupHeaderTemplateRight - groupHeaderTemplateLeft) (groupHeaderTemplateBottom-groupHeaderTemplateTop))
                 let leftOrTop = // left or top of initial item, after group header
                    match groupHeader, listType with
                        | Some _, Vertical -> leftOrTop + groupHeaderTemplateBottom - groupHeaderTemplateTop
                        | Some _, Horizontal -> leftOrTop + groupHeaderTemplateRight - groupHeaderTemplateLeft
                        | None, _ -> leftOrTop
                 let items, leftOrTop = // left or top of next group header
                     Seq.fold (fun (acc, y) curr ->
                         let item =
                             t.listMapTemplate (itemTemplate groupHeader) itemTemplateLeft
                                 itemTemplateTop itemTemplateRight itemTemplateBottom curr
                             |> Option.map (applyBounds
                                (-boundsXOffset/ratio + match listType with Horizontal -> y | Vertical -> 0R)
                                (-boundsYOffset/ratio + match listType with Vertical -> y | Horizontal -> 0R)
                                (itemTemplateRight - itemTemplateLeft)
                                (itemTemplateBottom - itemTemplateTop))
                         seq { yield! acc
                               match item with Some x -> x | None -> () },
                         match item, listType with
                         | Some _, Vertical -> y + itemTemplateBottom - itemTemplateTop
                         | Some _, Horizontal -> y + itemTemplateRight - itemTemplateLeft
                         | None, _ -> y
                      ) (Seq.empty, leftOrTop) items
                 seq {
                     yield! acc
                     match groupHeader with
                     | Some x -> x
                     | None -> ()
                     yield! items
                 }, leftOrTop
             ) (Seq.empty, 0R) source
             |> fst
             |> Seq.toList
         ), orientation = match listType with Horizontal -> ScrollOrientation.Horizontal | Vertical -> ScrollOrientation.Vertical
     , scrollTo = match listType with Horizontal -> scrollTo, 0., NotAnimated | Vertical -> 0., scrollTo, NotAnimated
     , scrolled = fun view -> (match listType with Horizontal -> view.ScrollX | Vertical -> view.ScrollY) |> updateScrollTo |> dispatch) |> applyBounds left top width height
 member t.list_ listType (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
     left top width height scrollTo updateScrollTo source =
     t.listGrouped_ listType (0R, 0R, 0R, 0R, fun () -> None)
         (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, fun _ -> itemTemplate)
         left top width height scrollTo updateScrollTo [(), source]
 member t.listGrouped
     listStyle (groupHeaderTemplateLeft, groupHeaderTemplateTop,
                groupHeaderTemplateRight, groupHeaderTemplateBottom, groupHeaderTemplate)
     (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
     left top width height scrollTo updateScrollTo source =
     t.listTemplate listStyle left top width height scrollTo updateScrollTo [
         for groupHeader, items in source do
             match t.listMapTemplate groupHeaderTemplate groupHeaderTemplateLeft
                 groupHeaderTemplateTop groupHeaderTemplateRight groupHeaderTemplateBottom groupHeader with
             | Some groupHeaderView -> groupHeaderView | None -> ()
             for item in items do
                 match t.listMapTemplate (itemTemplate groupHeader) itemTemplateLeft
                     itemTemplateTop itemTemplateRight itemTemplateBottom item with
                 | Some itemView -> itemView (*|> ViewElementExtensions.backgroundColor Color.Green*) | None -> ()
     ]
 member t.list listStyle
     (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
     left top width height scrollTo updateScrollTo source =
     t.listMapTemplate itemTemplate itemTemplateLeft itemTemplateTop itemTemplateRight itemTemplateBottom
     |> Seq.choose <| source
     |> List.ofSeq
     |> t.listTemplate listStyle left top width height scrollTo updateScrollTo
 member t.radioButtons whetherToHighlight unselectedFormatter unselectedFontSize unselectedTextColor
    unselectedBackColor unselectedBorderColor unselectedBorderThickness unselectedStyle
    selectedFormatter selectedFontSize selectedTextColor
    selectedBackColor selectedBorderColor selectedBorderThickness selectedStyle
    leftGenerator topGenerator width height (source:'Item seq) (item:'Item) updateItem =
    source |> Seq.mapi (fun i sourceItem ->
        (if whetherToHighlight (sourceItem, item) then
            t.buttonBordered (selectedFormatter sourceItem) selectedFontSize selectedTextColor
                selectedBackColor selectedBorderColor selectedBorderThickness selectedStyle
         else
            t.buttonBordered (unselectedFormatter sourceItem) unselectedFontSize unselectedTextColor
               unselectedBackColor unselectedBorderColor unselectedBorderThickness unselectedStyle
        ) (leftGenerator i) (topGenerator i) width height (updateItem sourceItem))
 member _.conditional_fabulous454workaround conditional ifTrue ifFalse =
    let workaround conditional = // fixes AQ0088, AQ0089, AQ0107
        if conditional then id else applyBoundsMap (fun _ -> Rectangle(-1., -1., 1., 1.))
    List.map (workaround conditional) ifTrue @ List.map (workaround <| not conditional) ifFalse
 member t.pickers (selectedId:SelectedPickerID<'ID>) updateSelectedId pickers =
    pickers
    |> Seq.mapFold (fun dropdown (Picker (currentId, buttonFunc, dropdownFunc)) ->
        buttonFunc <| updateSelectedId (SelectedPickerID currentId), 
        match selectedId with
        | SelectedPickerID selectedId when selectedId = currentId ->
            seq {
                t.background_escape <| updateSelectedId SelectedNoPickers
                yield! dropdownFunc <| updateSelectedId SelectedNoPickers
                buttonFunc <| updateSelectedId SelectedNoPickers
            }
        | _ -> dropdown
    ) Seq.empty
    |> (<||) Seq.append
 member t.picker (pickerId:'ID) buttonFormatter buttonFontSize buttonTextColor buttonBackColor buttonBorderColor buttonBorderThickness
     buttonStyle buttonLeft buttonTop buttonWidth buttonHeight itemFormatter itemFontSize itemTextColor itemHeight
     separatorColor separatorMargin (separatorThickness:float<R>) dropdownVisibleItemCount dropdownBackColor dropdownStyle
     (source:'Item seq) (item:'Item) updateItem =
     let xAdjust = -boundsXOffset / ratio
     let yAdjust = (buttonHeight:float<_>) / 2. - boundsYOffset / ratio
     let unitHeight = itemHeight + separatorThickness
     Picker(pickerId,
        t.buttonBordered (buttonFormatter item)
            buttonFontSize buttonTextColor buttonBackColor buttonBorderColor buttonBorderThickness buttonStyle
            buttonLeft buttonTop buttonWidth buttonHeight, fun updateDropdown -> [
        let width, height =
            buttonWidth,
                if dropdownVisibleItemCount =~ 0. then 0R
                else buttonHeight / 2. + unitHeight *
                        if int dropdownVisibleItemCount - 1 |> Seq.tryItem <| source |> Option.isSome
                        then dropdownVisibleItemCount
                        else Seq.length source |> float
        View.ScrollView(
            View.AbsoluteLayout(
                source |> Seq.mapi (fun i x -> seq {
                    t.buttonMultiMsg (itemFormatter x) itemFontSize itemTextColor transparent
                        (match dropdownStyle with
                        | Rectangular | Round | RoundRadius _ -> Rectangular
                        | RectangularLeft -> RectangularLeft
                        | RectangularBold | RoundBold -> RectangularBold) xAdjust
                        (float i * unitHeight + yAdjust) buttonWidth itemHeight [updateDropdown; updateItem x]
                    t.hLine separatorColor separatorThickness (separatorMargin + xAdjust)
                        (float i * unitHeight + itemHeight + yAdjust) (buttonWidth - separatorMargin - separatorMargin)
                }) |> Seq.concat |> List.ofSeq
            ), backgroundColor = colorHex dropdownBackColor
        ) |> applyBounds buttonLeft (buttonTop + buttonHeight / 2.) width height
        |> match dropdownStyle with
           | Rectangular | RectangularBold | RectangularLeft -> id
           | Round | RoundBold -> t.applyRounded (min width height / 2.)
           | RoundRadius r -> t.applyRounded r
     ])
 // https://github.com/fsprojects/Fabulous/issues/454
 /// Use this when multiple pickers are present in the same page!!
 member t.picker_ buttonFormatter buttonFontSize buttonTextColor buttonBackColor buttonBorderColor buttonBorderThickness
     buttonStyle buttonLeft buttonTop buttonWidth buttonHeight itemFormatter itemFontSize itemTextColor itemHeight
     separatorColor separatorMargin (separatorThickness:float<R>) dropdownVisibleItemCount dropdownBackColor dropdownStyle
     (source:'Item seq) dropdownOpened (item:'Item) (updateDropdown:'Message) updateItem =
     let xAdjust = -boundsXOffset / ratio
     let yAdjust = (buttonHeight:float<_>) / 2. - boundsYOffset / ratio
     let unitHeight = itemHeight + separatorThickness
     let baseButton =
        t.buttonBordered (buttonFormatter item)
            buttonFontSize buttonTextColor buttonBackColor buttonBorderColor buttonBorderThickness buttonStyle
            buttonLeft buttonTop buttonWidth buttonHeight updateDropdown
     {|
        BaseButton = baseButton
        ConditionalDropdown = [
            if dropdownOpened then
                let width, height =
                    buttonWidth, buttonHeight / 2. + unitHeight *
                        if int dropdownVisibleItemCount - 1 |> Seq.tryItem <| source |> Option.isSome
                        then dropdownVisibleItemCount
                        else Seq.length source |> float
                t.background_escape updateDropdown
                View.ScrollView(
                    View.AbsoluteLayout(
                        source |> Seq.mapi (fun i x -> seq {
                            t.buttonMultiMsg (itemFormatter x) itemFontSize itemTextColor transparent
                                (match dropdownStyle with
                                | Rectangular | Round | RoundRadius _ -> Rectangular
                                | RectangularLeft -> RectangularLeft
                                | RectangularBold | RoundBold -> RectangularBold) xAdjust
                                (float i * unitHeight + yAdjust) buttonWidth itemHeight [updateDropdown; updateItem x]
                            t.hLine separatorColor separatorThickness (separatorMargin + xAdjust)
                                (float i * unitHeight + itemHeight + yAdjust) (buttonWidth - separatorMargin - separatorMargin)
                        }) |> Seq.concat |> List.ofSeq
                    , isVisible = dropdownOpened), backgroundColor = colorHex dropdownBackColor
                ) |> applyBounds buttonLeft (buttonTop + buttonHeight / 2.) width height
                |> match dropdownStyle with
                   | Rectangular | RectangularBold | RectangularLeft -> id
                   | Round | RoundBold -> t.applyRounded (min width height / 2.)
                   | RoundRadius r -> t.applyRounded r
            baseButton
        ]
     |}
 static member val private touchAreaLocalInteractionIds = Collections.Generic.HashSet()
 member _.touchArea capturePassthroughInteractions left top width height handler =
    View.SKCanvasView(enableTouchEvents = true, touch = fun event ->
        event.Handled <- true
        if event.ActionType = Forms.SKTouchAction.Pressed then Views<_>.touchAreaLocalInteractionIds.Add event.Id |> ignore
        if (event.InContact || event.ActionType = Forms.SKTouchAction.Released)
           && (capturePassthroughInteractions || Views<_>.touchAreaLocalInteractionIds.Contains event.Id) then
            let density = Xamarin.Essentials.DeviceDisplay.MainDisplayInfo.Density
            handler (float event.Location.X / density / ratio + left)
                    (float event.Location.Y / density / ratio + top) event.ActionType
        match event.ActionType with
        | Forms.SKTouchAction.Released | Forms.SKTouchAction.Cancelled ->
            Views<_>.touchAreaLocalInteractionIds.Remove event.Id |> ignore
        | _ -> ()
    ) |> applyBounds left top width height
 member _.clipRect views =
     List.fold (fun rect (view:ViewElement) ->
         match rect, view.TryGetAttributeKeyed ViewAttributes.LayoutBoundsAttribKey with
         | ValueSome(rect:Rectangle), ValueSome r -> ValueSome (rect.Union r)
         | ValueNone, ValueSome r -> ValueSome r
         | _, ValueNone -> rect) ValueNone views
     |> ValueOption.defaultWith (fun () ->
         failwith "views do not have any elements with layoutBounds set. Not clippable.")
 member private _.combine (dx, dy) views =
     View.AbsoluteLayout(views |> List.map (applyBoundsMap <| fun r -> r.Offset (dx, dy)))
 member t.merge views =
     let rect = t.clipRect views
     t.combine (-rect.Location.X, -rect.Location.Y) views
     |> layoutBounds rect
 /// background does NOT support transparency
 member t.roundClip border thickness backgroundColor views =
     let clipRect = t.clipRect views
     View.Frame(content = View.Frame
        (content = t.combine (-clipRect.Location.X - thickness * ratio, -clipRect.Location.Y - thickness * ratio) views,
         backgroundColor = colorHex backgroundColor, isClippedToBounds = true,
         padding = Thickness 0., cornerRadius = min clipRect.Width clipRect.Height / 2. - thickness * ratio),
 
         backgroundColor = colorHex border, isClippedToBounds = true,
         padding = Thickness (thickness * ratio, thickness * ratio, thickness * ratio * 2., thickness * ratio),
         cornerRadius = min clipRect.Width clipRect.Height / 2.)
     |> layoutBounds clipRect
 member _.drawingConstrained left top width height drawings =
     if Seq.isEmpty drawings then View.ContentView() // Don't waste resources...
     else
         let skMeasure (n:float<R>) = float32 n
         View.SKCanvasView(invalidate = true, paintSurface = fun args ->
             let canvas = args.Surface.Canvas
             canvas.Clear()
             min (float canvas.DeviceClipBounds.Width / width) (float canvas.DeviceClipBounds.Height / height)
             |> float32
             |> canvas.Scale
             canvas.Translate(-skMeasure left, -skMeasure top)
             use paint = new SKPaint()
             let rec draw =
                 function
                 | DrawTransform(matrix, drawing) ->
                     use _r = new SKAutoCanvasRestore(canvas)
                     let mutable m = canvas.TotalMatrix
                     SKMatrix.PreConcat(&m, matrix)
                     canvas.SetMatrix m
                     draw drawing
                 | DrawTransformMulti(matrix, drawings) ->
                     use _r = new SKAutoCanvasRestore(canvas)
                     let mutable m = canvas.TotalMatrix
                     SKMatrix.PreConcat(&m, matrix)
                     canvas.SetMatrix m
                     Seq.iter draw drawings
                 | DrawElement drawer ->
                     drawer canvas paint
             Seq.iter draw drawings // Cover first with last
       ) |> applyBounds left top width height
 member t.drawing drawings = t.drawingConstrained 0R 0R totalWidth totalHeight drawings