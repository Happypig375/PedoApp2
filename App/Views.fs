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
 let inline (=~) (x:^a) (y:^a) = abs (x / GenericOneWithTypeOf x - y / GenericOneWithTypeOf y |> float) <= 0.0001
 let rgba(r, g, b, a) = (a * 255. |> int |> (+) 1 <<< 24) + (r <<< 16) + (g <<< 8) + b
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
 let applyReplace (attribKey:AttributeKey<'T>) f (view:ViewElement) =
    for i in 0 .. view.AttributesKeyed.Length - 1 do
        let kv = view.AttributesKeyed.[i]
        if kv.Key = attribKey.KeyValue
        then view.AttributesKeyed.[i] <- System.Collections.Generic.KeyValuePair
                (kv.Key, kv.Value :?> 'T |> f |> box<'T>)
    view
 let applyBoundsMap = applyReplace ViewAttributes.LayoutBoundsAttribKey
 let applyEffect effect = applyReplace ViewAttributes.EffectsAttribKey (Array.append [|effect|])
 let applyOpacity value = opacity value
 let applyButtonLeftAlignment e = effects [Effects.buttonLeftAlign] e
 let applyTextBold e = fontAttributes FontAttributes.Bold e
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
     let private assign colorHex thickness (paint:SKPaint) =
         paint.StrokeWidth <- skMeasure thickness
         paint.Color <- hexToUint32 colorHex |> SKColor.op_Implicit
         paint.Style <- if thickness = thicknessFill then SKPaintStyle.Fill else SKPaintStyle.Stroke
         paint.IsAntialias <- true
         paint
     let translate x y drawing =
         DrawTransform(SKMatrix.MakeTranslation(skMeasure x, skMeasure y), drawing)
     let group x y drawings =
         DrawTransformMulti(SKMatrix.MakeTranslation(skMeasure x, skMeasure y), drawings)
     let circle colorHex thickness x y r =
         DrawElement(fun canvas paint ->
            canvas.DrawCircle(skMeasure (x+r), skMeasure (y+r), skMeasure r, assign colorHex thickness paint))
     let rect colorHex thickness x y w h =
         DrawElement(fun canvas paint ->
            canvas.DrawRect(skMeasure x, skMeasure y, skMeasure w, skMeasure h, assign colorHex thickness paint))
     let roundRect colorHex thickness x y w h r =
         DrawElement(fun canvas paint ->
            canvas.DrawRoundRect(skMeasure x, skMeasure y, skMeasure w, skMeasure h, skMeasure r, skMeasure r, assign colorHex thickness paint))
     let polygon colorHex thickness points =
         DrawElement(fun canvas paint ->
            use path = new SKPath()
            path.AddPoly(points)
            canvas.DrawPath(path, assign colorHex thickness paint))
     let path colorHex thickness path =
         DrawElement(fun canvas paint ->
            use path = SKPath.ParseSvgPathData path in canvas.DrawPath(path, assign colorHex thickness paint))
 let _finalizeToPage dispatch updateScreenSize views =
     Seq.toList views
     |> fun l -> View.AbsoluteLayout l
     |> fun a -> View.ContentPage(a, sizeAllocated = (updateScreenSize >> dispatch))
 type [<Struct>] SelectedPickerID<'ID when 'ID : equality> = SelectedPickerID of 'ID | SelectedNoPickers
 type [<NoComparison; NoEquality>] Picker<'ID, 'UpdateDropdown when 'ID : equality> =
    Picker of 'ID * baseButton:('UpdateDropdown -> ViewElement) * dropdown:('UpdateDropdown -> ViewElement list)
 type [<NoComparison; NoEquality>] ViewsEnvironment<'Message> = {
     Dispatch : 'Message -> unit
     ScreenWidth : float
     ScreenHeight : float
 }
open Views
/// Views from SVG data
type Views<'Message>(env : ViewsEnvironment<'Message>, totalX : float<R>, totalY : float<R>) =
 /// Screen units to layout units ratio 
 let ratio = min (env.ScreenWidth / totalX) (env.ScreenHeight / totalY)
 // The iPhone 6S sometimes makes views with width/height less than 0.5 device units disappear
 let guardEpsilons n = if 0. < n && n < 0.5 then 0.5 else n
 let boundsXOffset, boundsYOffset =
    (env.ScreenWidth - totalX * ratio) / 2., (env.ScreenHeight - totalY * ratio) / 2. 
 let background_bounds x y w h =
     let pos posComp offset =
         if posComp =~ 0R then 0. else posComp * ratio + offset
     let size posComp sizeComp offset total =
         sizeComp * ratio
         + if posComp =~ 0R then offset else 0.
         + if posComp + sizeComp =~ total then offset else 0.
         |> guardEpsilons
     Rectangle (
         pos x boundsXOffset,
         pos y boundsYOffset,
         size x w boundsXOffset totalX,
         size y h boundsYOffset totalY)
     |> layoutBounds
 let background_hBounds y h = background_bounds 0R y totalX h
 let background_vBounds x w = background_bounds x 0R w totalY
 let applyBoundsWH x y width height =
     let rect = Rectangle(x * ratio + boundsXOffset, y * ratio + boundsYOffset, guardEpsilons width, guardEpsilons height)
     layoutBounds rect
 let applyBounds x y width height = applyBoundsWH x y (width * ratio) (height * ratio)
 member _.applyBoundsOn = applyBounds
 member _.applyRounded radius (e:ViewElement) =
     View.Frame(content = e, padding = Thickness 0., isClippedToBounds = true, hasShadow = false,
                cornerRadius = radius * ratio, backgroundColor = Color.Transparent)
     |> applyBoundsFrom e
 // https://github.com/xamarin/Xamarin.Forms/issues/1695 ...
 /// Doesn't support transparency on backColor!!!
 member t.applyRoundedBorder_NoBackColorTransparency radius backColor borderColor borderWidth e =
     e |> t.applyRounded (radius - borderWidth) |> backgroundColor (colorHex backColor) |> 
     t.applyRounded radius |> backgroundColor (colorHex borderColor) |> padding (borderWidth * ratio |> Thickness)
     |> applyBoundsFrom e
 //// Doesn't work properly...
 ///// Doesn't support clipping the containing element to frame bounds!!!
 //let applyRoundedBorder_NoClip x y w h radius backColor borderColor borderWidth e =
 //    [
 //        View.Button(backgroundColor = colorHex backColor, cornerRadius = int (radius * ratio),
 //            borderWidth = borderWidth * ratio, borderColor = colorHex borderColor
 //          , effects = [Effects.zeroPadding()]) |> applyBounds x y w h
 //        e
 //    ]
 member _.info_totalX = totalX
 member _.info_totalY = totalY
 member _.info_isLandscape = env.ScreenWidth > env.ScreenHeight
 member _.background_hRect hex y h = View.BoxView(backgroundColor = colorHex hex) |> background_hBounds y h
 member _.background_vRect hex x w = View.BoxView(backgroundColor = colorHex hex) |> background_vBounds x w
 member _.background_oRect hex x y w h = View.BoxView(backgroundColor = colorHex hex) |> background_bounds x y w h
 member _.background_roundRectTop hex h r = View.BoxView(backgroundColor = colorHex hex, cornerRadius = CornerRadius (0., 0., r * ratio, r * ratio)) |> background_hBounds 0R h
 member t.background_rect hex = t.background_oRect hex 0R 0R totalX totalY
 member t.background_hLine hex thickness y = t.background_hRect hex (y - thickness / 2.) thickness
 member t.background_hLineFromLeft hex thickness x y = t.background_oRect hex 0R (y - thickness / 2.) x thickness
 member t.background_hLineFromRight hex thickness x y = t.background_oRect hex x (y - thickness / 2.) (totalX-x) thickness
 member _.background_hImage (source:ImageSource) y h = View.Image(Image.ImageSource source) |> background_hBounds y h
 member _.background_escape msg = // e.g. to close current dropdown
     View.ContentView(gestureRecognizers = [
         View.TapGestureRecognizer(fun () -> env.Dispatch msg)
     ]) |> background_bounds 0R 0R totalX totalY
 
 member _.group dx dy = List.map (applyBoundsMap (fun x -> Rectangle(x.X + dx * ratio, x.Y + dy * ratio, x.Width, x.Height)))
 member _.text text fz hexColor x y =
     View.Label(text, textColor = colorHex hexColor, fontSize = FontSize.Size (fz * ratio))
     |> applyBoundsWH x (y - fz) AbsoluteLayout.AutoSize AbsoluteLayout.AutoSize
 member _.textCenterSpan text fz hexColor x y right =
     View.Label(text, horizontalTextAlignment = TextAlignment.Center, textColor = colorHex hexColor, fontSize = FontSize.Size (fz * ratio))
     |> applyBoundsWH x (y - fz) ((right-x)*ratio) AbsoluteLayout.AutoSize
 member _.textRightSpan text fz hexColor x y right =
     View.Label(text, horizontalTextAlignment = TextAlignment.End, textColor = colorHex hexColor, fontSize = FontSize.Size (fz * ratio))
     |> applyBoundsWH x (y - fz) ((right-x)*ratio) AbsoluteLayout.AutoSize
 member _.textCenterRect text fz hexColor x y w h =
     View.Label(text, horizontalTextAlignment = TextAlignment.Center, verticalTextAlignment = TextAlignment.Center, textColor = colorHex hexColor,
         fontSize = FontSize.Size (fz * ratio)) |> applyBounds x y w h
 member _.textCenterMargin text fz hexColor hMargin y =
     View.Label(text, horizontalTextAlignment = TextAlignment.Center, textColor = colorHex hexColor, fontSize = FontSize.Size (fz * ratio))
     |> applyBoundsWH hMargin (y - fz) ((totalX - hMargin * 2.) * ratio) AbsoluteLayout.AutoSize
 member t.textCenter text fz hexColor y = t.textCenterMargin text fz hexColor 0R y
 member t.textScroll text fz hexColor x y w h = View.ScrollView(t.textCenter text fz hexColor 0R) |> applyBounds x y w h
 member _.textEdit keyboard text fontSize textColor placeholder placeholderColor x y right textChanged =
     let keyboard, isPassword =
         match keyboard:Keyboard with
         | :? Internals.CustomKeyboard as custom when custom.Flags = passwordKeyboardFlags -> Keyboard.Default, true
         | keyboard -> keyboard, false
     View.Entry(text, fontSize = FontSize.Size (fontSize * ratio), textColor = colorHex textColor, keyboard = keyboard, 
         placeholder = placeholder, placeholderColor = colorHex placeholderColor, backgroundColor = colorHex transparent,
         isPassword = isPassword, textChanged = fun args -> args.NewTextValue |> textChanged |> env.Dispatch
       , effects = [Effects.borderless]
     ) |> applyBounds (x - 1R) (y - fontSize - 4R) (right - x + 1R) (fontSize + 11R)
 member t.textEditCenter keyboard text fontSize textColor placeholder placeholderColor left top w h textChanged =
     t.textEdit keyboard text fontSize textColor placeholder placeholderColor 0R 0R 0R textChanged
     |> horizontalTextAlignment TextAlignment.Center
     |> verticalTextAlignment TextAlignment.Center
     |> applyEffect Effects.xf7538_AndroidEntryCenterAlign
     |> applyBounds left top w h
 member _.rect color x y w h = View.BoxView(backgroundColor = colorHex color) |> applyBounds x y w h
 member _.roundRect color x y w h r = View.BoxView(backgroundColor = colorHex color, cornerRadius = CornerRadius (r * ratio)) |> applyBounds x y w h
 member _.shadow color x y w h =
     let shadowThickness = 10R
     View.Frame(hasShadow = true, backgroundColor = colorHex color, margin = Thickness (shadowThickness * ratio))
     |> applyBounds (x - shadowThickness) (y - shadowThickness) (w + shadowThickness * 2.) (h + shadowThickness * 2.)
 member t.roundShadow color x y w h r = t.shadow color x y w h |> frameCornerRadius (r * ratio)
 /// x2 in svg = width here
 member _.hLine hex thickness x y w = View.BoxView(backgroundColor = colorHex hex) |> applyBounds x (y-thickness / 2.) w thickness 
 // Use loadingPlaceholder to counter previous image still being present after Fabulous reuses the image view: empty.png is a 1×1 image containing one transparent pixel
 member _.image x y w h file =
     View.CachedImage(source = Image.ImageSource file, aspect = Aspect.AspectFit, loadingPlaceholder = Image.ImageSource Images.``empty.png``) |> applyBounds x y w h
 member t.imageAround midX midY w h = t.image (midX - w / 2.) (midY - h / 2.) w h
 member _.imageFill x y w h (file:ImageSource) =
     View.CachedImage(source = Image.ImageSource file, aspect = Aspect.AspectFill, loadingPlaceholder = Image.ImageSource Images.``empty.png``) |> applyBounds x y w h
 member _.buttonInvisible left top w h msg = 
     View.Button("", fun () -> env.Dispatch msg
       , padding = Thickness 0., backgroundColor = Color.Transparent, borderColor = Color.Transparent,
       effects = [Effects.zeroPadding])
       |> applyBounds left top w h
 member _.buttonInvisibleMultiPress pressCount left top w h msg = 
     View.ContentView(gestureRecognizers = [View.TapGestureRecognizer(numberOfTapsRequired = pressCount, command = fun () -> env.Dispatch msg)])
     |> applyBounds left top w h
 member _.buttonMultiMsg text fontSize textColor backColor style left top w h msgs =
     View.Button(text, fun () -> Seq.iter env.Dispatch msgs
       , textColor = colorHex textColor, fontSize = FontSize.Size (fontSize * ratio), padding = Thickness 0.,
         backgroundColor = colorHex backColor, borderWidth = 0., borderColor = Color.Transparent,
         fontAttributes = match style with
                          | Round | RoundRadius _ | Rectangular | RectangularLeft -> FontAttributes.None
                          | RoundBold | RectangularBold -> FontAttributes.Bold
       , cornerRadius = match style with
                        | Round | RoundBold -> min w h / 2. * ratio |> int
                        | RoundRadius r -> r * ratio |> int
                        | Rectangular | RectangularBold | RectangularLeft -> 0
       , effects = [Effects.zeroPadding])
       |> applyBounds left top w h
 member t.button text fontSize textColor backColor style left top w h msg =
     t.buttonMultiMsg text fontSize textColor backColor style left top w h [msg]
 member t.buttonBordered text fontSize textColor backColor borderColor borderThickness style left top w h msg =
     t.button text fontSize textColor backColor style left top w h msg
     |> ViewElementExtensions.borderColor (colorHex borderColor) |> borderWidth (borderThickness * ratio)
 member t.textBordered text fontSize textColor backColor borderColor borderThickness style left top w h =
     t.buttonBordered text fontSize textColor backColor
         borderColor borderThickness style left top w h Unchecked.defaultof<_>
     |> applyReplace ViewAttributes.CommandAttribKey (fun _ () -> ())
     |> isTabStop false
 member t.imageButton x y w h msg (padding:float<_>) (file:ImageSource) = [
     t.image (x+padding) (y+padding) (w-padding*2.) (h-padding*2.) file
     t.buttonInvisible x y w h msg
 ]
 member t.circle color left top r = t.roundRect color left top (r*2.) (r*2.) r
 member t.circleBordered backColor borderColor borderThickness left top (r:float<_>) =
     t.textBordered "" 0R transparent backColor borderColor borderThickness Round left top (r*2.) (r*2.)
 member _.activityIndicator hexColor x y w h =
     View.ActivityIndicator(true, color = colorHex hexColor) |> applyBounds x y w h
 member private _.listMapTemplate template templateLeft templateTop templateRight templateBottom templateArg =
     template templateArg
     |> Option.map (fun views ->
         View.AbsoluteLayout(views |> Seq.map (applyBoundsMap(fun rect ->
                 Rectangle(rect.Left - boundsXOffset - templateLeft * ratio,
                             rect.Top - boundsYOffset - templateTop * ratio,
                             rect.Width, rect.Height))) |> List.ofSeq,
             width = (templateRight - templateLeft) * ratio,
             height = (templateBottom - templateTop) * ratio))
 member private _.listTemplate listStyle x y w h scrollTo updateScrollTo items =
     ignore (updateScrollTo : 'b -> 'a)
     ignore (scrollTo : 'b)
     View.CollectionView(items,
         itemsLayout = ListStyle.toItemsLayout ratio listStyle,
         itemSizingStrategy = ItemSizingStrategy.MeasureFirstItem
         // // Scrolling memorization for UWP causes crashes, unfortunately:
         // // https://github.com/xamarin/Xamarin.Forms/issues/8508
         // , scrolled = fun e -> e.FirstVisibleItemIndex |> updateScrollTo |> dispatch
         // , created = fun c ->
         //     if Xamarin.Essentials.DeviceInfo.Platform <> Xamarin.Essentials.DevicePlatform.UWP
         //     then c.ScrollTo(scrollTo, -1, ScrollToPosition.MakeVisible, false)
     ) |> applyBounds x y w h
 /// Temporary implementation of list using ScrollView instead of CollectionView,
 /// used to evade https://github.com/fsprojects/Fabulous/issues/609
 member t.listGrouped_ listType (groupHeaderTemplateLeft, groupHeaderTemplateTop,
                                     groupHeaderTemplateRight, groupHeaderTemplateBottom, groupHeaderTemplate)
         (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
         x y w h scrollTo updateScrollTo source =
     View.ScrollView (
         View.AbsoluteLayout (
             dependsOn
                 (source, groupHeaderTemplateLeft, groupHeaderTemplateTop, groupHeaderTemplateRight, groupHeaderTemplateBottom, groupHeaderTemplate,
                  itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
                 <| fun _ _ ->
             Seq.fold (fun (acc, y) (groupHeader, items) ->
                 let groupHeader =
                     t.listMapTemplate groupHeaderTemplate groupHeaderTemplateLeft
                         groupHeaderTemplateTop groupHeaderTemplateRight groupHeaderTemplateBottom groupHeader
                     |> Option.map (
                        applyBounds
                            (-boundsXOffset/ratio + match listType with Horizontal -> y | Vertical -> 0R)
                            (-boundsYOffset/ratio + match listType with Vertical -> y | Horizontal -> 0R)
                            (groupHeaderTemplateRight - groupHeaderTemplateLeft) (groupHeaderTemplateBottom-groupHeaderTemplateTop))
                 let y = match groupHeader, listType with
                         | Some _, Vertical -> y + groupHeaderTemplateBottom - groupHeaderTemplateTop
                         | Some _, Horizontal -> y + groupHeaderTemplateRight - groupHeaderTemplateLeft
                         | None, _ -> y
                 let items, y =
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
                      ) (Seq.empty, y) items
                 seq {
                     yield! acc
                     match groupHeader with
                     | Some x -> x
                     | None -> ()
                     yield! items
                 }, y
             ) (Seq.empty, 0R) source
             |> fst
             |> Seq.toList
         ), orientation = match listType with Horizontal -> ScrollOrientation.Horizontal | Vertical -> ScrollOrientation.Vertical
     , scrollTo = match listType with Horizontal -> scrollTo, 0., NotAnimated | Vertical -> 0., scrollTo, NotAnimated
     , scrolled = fun e -> (match listType with Horizontal -> e.ScrollX | Vertical -> e.ScrollY) |> updateScrollTo |> env.Dispatch) |> applyBounds x y w h
 member t.list_ listType (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
     x y w h scrollTo updateScrollTo source =
     t.listGrouped_ listType (0R, 0R, 0R, 0R, fun () -> None)
         (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, fun _ -> itemTemplate)
         x y w h scrollTo updateScrollTo [(), source]
 member t.listGrouped
     listStyle (groupHeaderTemplateLeft, groupHeaderTemplateTop,
                groupHeaderTemplateRight, groupHeaderTemplateBottom, groupHeaderTemplate)
     (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
     x y w h scrollTo updateScrollTo source =
     t.listTemplate listStyle x y w h scrollTo updateScrollTo [
         for groupHeader, items in source do
             match t.listMapTemplate groupHeaderTemplate groupHeaderTemplateLeft
                 groupHeaderTemplateTop groupHeaderTemplateRight groupHeaderTemplateBottom groupHeader with
             | Some x -> x | None -> ()
             for item in items do
                 match t.listMapTemplate (itemTemplate groupHeader) itemTemplateLeft
                     itemTemplateTop itemTemplateRight itemTemplateBottom item with
                 | Some x -> x (*|> ViewElementExtensions.backgroundColor Color.Green*) | None -> ()
     ]
 member t.list listStyle
     (itemTemplateLeft, itemTemplateTop, itemTemplateRight, itemTemplateBottom, itemTemplate)
     x y w h scrollTo updateScrollTo source =
     t.listMapTemplate itemTemplate itemTemplateLeft itemTemplateTop itemTemplateRight itemTemplateBottom
     |> Seq.choose <| source
     |> List.ofSeq
     |> t.listTemplate listStyle x y w h scrollTo updateScrollTo
 member t.radioButtons whetherToHighlight unselectedFormatter unselectedFontSize unselectedTextColor
    unselectedBackColor unselectedBorderColor unselectedBorderThickness unselectedStyle
    selectedFormatter selectedFontSize selectedTextColor
    selectedBackColor selectedBorderColor selectedBorderThickness selectedStyle
    leftGenerator topGenerator w h (source:'Item seq) (item:'Item) updateItem =
    source |> Seq.mapi (fun i sourceItem ->
        (if whetherToHighlight (sourceItem, item) then
            t.buttonBordered (selectedFormatter sourceItem) selectedFontSize selectedTextColor
                selectedBackColor selectedBorderColor selectedBorderThickness selectedStyle
         else
            t.buttonBordered (unselectedFormatter sourceItem) unselectedFontSize unselectedTextColor
               unselectedBackColor unselectedBorderColor unselectedBorderThickness unselectedStyle
        ) (leftGenerator i) (topGenerator i) w h (updateItem sourceItem))
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
 member t.roundClip border thickness background views =
     let rect = t.clipRect views
     View.Frame(content = View.Frame
        (content = t.combine (-rect.Location.X - thickness * ratio, -rect.Location.Y - thickness * ratio) views,
         backgroundColor = colorHex background, isClippedToBounds = true,
         padding = Thickness 0., cornerRadius = min rect.Width rect.Height / 2. - thickness * ratio),
 
         backgroundColor = colorHex border, isClippedToBounds = true,
         padding = Thickness (thickness * ratio, thickness * ratio, thickness * ratio * 2., thickness * ratio),
         cornerRadius = min rect.Width rect.Height / 2.)
     |> layoutBounds rect
 member _.drawingConstrained x y w h drawings =
     if Seq.isEmpty drawings then View.ContentView() // Don't waste resources...
     else
         let skMeasure (n:float<R>) = float32 n
         View.SKCanvasView(invalidate = true, paintSurface = fun args ->
             let canvas = args.Surface.Canvas
             canvas.Clear()
             min (float canvas.DeviceClipBounds.Width / w) (float canvas.DeviceClipBounds.Height / h)
             |> float32
             |> canvas.Scale
             canvas.Translate(-skMeasure x, -skMeasure y)
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
       ) |> applyBounds x y w h
 member t.drawing drawings = t.drawingConstrained 0R 0R totalX totalY drawings