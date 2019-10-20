// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace Fabulous.iOS

#nowarn "59" // cast always holds
#nowarn "66" // cast always holds
#nowarn "67" // cast always holds

open Fabulous

module ViewAttributes =
    let UIViewBackgroundColorAttribKey : AttributeKey<_> = AttributeKey<_>("UIViewBackgroundColor")
    let UIViewFrameAttribKey : AttributeKey<_> = AttributeKey<_>("UIViewFrame")
    let UIViewSubviewsAttribKey : AttributeKey<_> = AttributeKey<_>("UIViewSubviews")
    let TitleAttribKey : AttributeKey<_> = AttributeKey<_>("Title")
    let UIViewControllerViewAttribKey : AttributeKey<_> = AttributeKey<_>("UIViewControllerView")
    let UIButtonTitleColorAttribKey : AttributeKey<_> = AttributeKey<_>("UIButtonTitleColor")

type ViewBuilders() =
    /// Builds the attributes for a UIView in the view
    static member inline BuildUIView(attribCount: int,
                                     ?backgroundColor: UIKit.UIColor,
                                     ?frame: CoreGraphics.CGRect,
                                     ?subviews: ViewElement list) = 

        let attribCount = match backgroundColor with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match frame with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match subviews with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = new AttributesBuilder(attribCount)
        match backgroundColor with None -> () | Some v -> attribBuilder.Add(ViewAttributes.UIViewBackgroundColorAttribKey, (v)) 
        match frame with None -> () | Some v -> attribBuilder.Add(ViewAttributes.UIViewFrameAttribKey, (v)) 
        match subviews with None -> () | Some v -> attribBuilder.Add(ViewAttributes.UIViewSubviewsAttribKey, Array.ofList(v)) 
        attribBuilder

    static member CreateUIView () : UIKit.UIView =
        new UIKit.UIView()

    static member UpdateUIView (prevOpt: ViewElement voption, curr: ViewElement, target: UIKit.UIView) = 
        let mutable prevUIViewBackgroundColorOpt = ValueNone
        let mutable currUIViewBackgroundColorOpt = ValueNone
        let mutable prevUIViewFrameOpt = ValueNone
        let mutable currUIViewFrameOpt = ValueNone
        let mutable prevUIViewSubviewsOpt = ValueNone
        let mutable currUIViewSubviewsOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.UIViewBackgroundColorAttribKey.KeyValue then 
                currUIViewBackgroundColorOpt <- ValueSome (kvp.Value :?> UIKit.UIColor)
            if kvp.Key = ViewAttributes.UIViewFrameAttribKey.KeyValue then 
                currUIViewFrameOpt <- ValueSome (kvp.Value :?> CoreGraphics.CGRect)
            if kvp.Key = ViewAttributes.UIViewSubviewsAttribKey.KeyValue then 
                currUIViewSubviewsOpt <- ValueSome (kvp.Value :?> ViewElement array)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.UIViewBackgroundColorAttribKey.KeyValue then 
                    prevUIViewBackgroundColorOpt <- ValueSome (kvp.Value :?> UIKit.UIColor)
                if kvp.Key = ViewAttributes.UIViewFrameAttribKey.KeyValue then 
                    prevUIViewFrameOpt <- ValueSome (kvp.Value :?> CoreGraphics.CGRect)
                if kvp.Key = ViewAttributes.UIViewSubviewsAttribKey.KeyValue then 
                    prevUIViewSubviewsOpt <- ValueSome (kvp.Value :?> ViewElement array)
        // Update properties
        match prevUIViewBackgroundColorOpt, currUIViewBackgroundColorOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.BackgroundColor <-  currValue
        | ValueSome _, ValueNone -> target.BackgroundColor <- UIKit.UIColor.Clear
        | ValueNone, ValueNone -> ()
        match prevUIViewFrameOpt, currUIViewFrameOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Frame <-  currValue
        | ValueSome _, ValueNone -> target.Frame <- CoreGraphics.CGRect(0., 0., 0., 0.)
        | ValueNone, ValueNone -> ()
        ViewUpdaters.updateUIViewSubviews prevUIViewSubviewsOpt currUIViewSubviewsOpt target
            (fun _ _ _ -> ())

    static member inline ConstructUIView(?backgroundColor: UIKit.UIColor,
                                         ?frame: CoreGraphics.CGRect,
                                         ?subviews: ViewElement list) = 

        let attribBuilder = ViewBuilders.BuildUIView(0,
                               ?backgroundColor=backgroundColor,
                               ?frame=frame,
                               ?subviews=subviews)

        ViewElement.Create<UIKit.UIView>(ViewBuilders.CreateUIView, (fun prevOpt curr target -> ViewBuilders.UpdateUIView(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a UIViewController in the view
    static member inline BuildUIViewController(attribCount: int,
                                               ?title: string,
                                               ?view: ViewElement) = 

        let attribCount = match title with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match view with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = new AttributesBuilder(attribCount)
        match title with None -> () | Some v -> attribBuilder.Add(ViewAttributes.TitleAttribKey, (v)) 
        match view with None -> () | Some v -> attribBuilder.Add(ViewAttributes.UIViewControllerViewAttribKey, (v)) 
        attribBuilder

    static member CreateUIViewController () : UIKit.UIViewController =
        new UIKit.UIViewController()

    static member UpdateUIViewController (prevOpt: ViewElement voption, curr: ViewElement, target: UIKit.UIViewController) = 
        let mutable prevTitleOpt = ValueNone
        let mutable currTitleOpt = ValueNone
        let mutable prevUIViewControllerViewOpt = ValueNone
        let mutable currUIViewControllerViewOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.TitleAttribKey.KeyValue then 
                currTitleOpt <- ValueSome (kvp.Value :?> string)
            if kvp.Key = ViewAttributes.UIViewControllerViewAttribKey.KeyValue then 
                currUIViewControllerViewOpt <- ValueSome (kvp.Value :?> ViewElement)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.TitleAttribKey.KeyValue then 
                    prevTitleOpt <- ValueSome (kvp.Value :?> string)
                if kvp.Key = ViewAttributes.UIViewControllerViewAttribKey.KeyValue then 
                    prevUIViewControllerViewOpt <- ValueSome (kvp.Value :?> ViewElement)
        // Update properties
        match prevTitleOpt, currTitleOpt with
        | ValueSome prevValue, ValueSome currValue when prevValue = currValue -> ()
        | _, ValueSome currValue -> target.Title <-  currValue
        | ValueSome _, ValueNone -> target.Title <- null
        | ValueNone, ValueNone -> ()
        match prevUIViewControllerViewOpt, currUIViewControllerViewOpt with
        // For structured objects, dependsOn on reference equality
        | ValueSome prevValue, ValueSome newValue when identical prevValue newValue -> ()
        | ValueSome prevValue, ValueSome newValue when canReuseView prevValue newValue ->
            newValue.UpdateIncremental(prevValue, target.View)
        | _, ValueSome newValue ->
            target.View <- (newValue.Create() :?> UIKit.UIView)
        | ValueSome _, ValueNone ->
            target.View <- null
        | ValueNone, ValueNone -> ()

    static member inline ConstructUIViewController(?title: string,
                                                   ?view: ViewElement) = 

        let attribBuilder = ViewBuilders.BuildUIViewController(0,
                               ?title=title,
                               ?view=view)

        ViewElement.Create<UIKit.UIViewController>(ViewBuilders.CreateUIViewController, (fun prevOpt curr target -> ViewBuilders.UpdateUIViewController(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a UIStackView in the view
    static member inline BuildUIStackView(attribCount: int,
                                          ?backgroundColor: UIKit.UIColor,
                                          ?frame: CoreGraphics.CGRect,
                                          ?subviews: ViewElement list) = 
        let attribBuilder = ViewBuilders.BuildUIView(attribCount, ?backgroundColor=backgroundColor, ?frame=frame, ?subviews=subviews)
        attribBuilder

    static member CreateUIStackView () : UIKit.UIStackView =
        new UIKit.UIStackView()

    static member UpdateUIStackView (prevOpt: ViewElement voption, curr: ViewElement, target: UIKit.UIStackView) = 
        ViewBuilders.UpdateUIView (prevOpt, curr, target)

    static member inline ConstructUIStackView(?backgroundColor: UIKit.UIColor,
                                              ?frame: CoreGraphics.CGRect,
                                              ?subviews: ViewElement list) = 

        let attribBuilder = ViewBuilders.BuildUIStackView(0,
                               ?backgroundColor=backgroundColor,
                               ?frame=frame,
                               ?subviews=subviews)

        ViewElement.Create<UIKit.UIStackView>(ViewBuilders.CreateUIStackView, (fun prevOpt curr target -> ViewBuilders.UpdateUIStackView(prevOpt, curr, target)), attribBuilder)

    /// Builds the attributes for a UIButton in the view
    static member inline BuildUIButton(attribCount: int,
                                       ?title: string,
                                       ?titleColor: UIKit.UIColor,
                                       ?backgroundColor: UIKit.UIColor,
                                       ?frame: CoreGraphics.CGRect,
                                       ?subviews: ViewElement list) = 

        let attribCount = match title with Some _ -> attribCount + 1 | None -> attribCount
        let attribCount = match titleColor with Some _ -> attribCount + 1 | None -> attribCount

        let attribBuilder = ViewBuilders.BuildUIView(attribCount, ?backgroundColor=backgroundColor, ?frame=frame, ?subviews=subviews)
        match title with None -> () | Some v -> attribBuilder.Add(ViewAttributes.TitleAttribKey, (v)) 
        match titleColor with None -> () | Some v -> attribBuilder.Add(ViewAttributes.UIButtonTitleColorAttribKey, (v)) 
        attribBuilder

    static member CreateUIButton () : UIKit.UIButton =
        new UIKit.UIButton(UIKit.UIButtonType.System)

    static member UpdateUIButton (prevOpt: ViewElement voption, curr: ViewElement, target: UIKit.UIButton) = 
        let mutable prevTitleOpt = ValueNone
        let mutable currTitleOpt = ValueNone
        let mutable prevUIButtonTitleColorOpt = ValueNone
        let mutable currUIButtonTitleColorOpt = ValueNone
        for kvp in curr.AttributesKeyed do
            if kvp.Key = ViewAttributes.TitleAttribKey.KeyValue then 
                currTitleOpt <- ValueSome (kvp.Value :?> string)
            if kvp.Key = ViewAttributes.UIButtonTitleColorAttribKey.KeyValue then 
                currUIButtonTitleColorOpt <- ValueSome (kvp.Value :?> UIKit.UIColor)
        match prevOpt with
        | ValueNone -> ()
        | ValueSome prev ->
            for kvp in prev.AttributesKeyed do
                if kvp.Key = ViewAttributes.TitleAttribKey.KeyValue then 
                    prevTitleOpt <- ValueSome (kvp.Value :?> string)
                if kvp.Key = ViewAttributes.UIButtonTitleColorAttribKey.KeyValue then 
                    prevUIButtonTitleColorOpt <- ValueSome (kvp.Value :?> UIKit.UIColor)
        // Update inherited members
        ViewBuilders.UpdateUIView (prevOpt, curr, target)
        // Update properties
        ViewUpdaters.updateUIButtonTitle prevTitleOpt currTitleOpt target
        ViewUpdaters.updateUIButtonTitleColor prevUIButtonTitleColorOpt currUIButtonTitleColorOpt target

    static member inline ConstructUIButton(?title: string,
                                           ?titleColor: UIKit.UIColor,
                                           ?backgroundColor: UIKit.UIColor,
                                           ?frame: CoreGraphics.CGRect,
                                           ?subviews: ViewElement list) = 

        let attribBuilder = ViewBuilders.BuildUIButton(0,
                               ?title=title,
                               ?titleColor=titleColor,
                               ?backgroundColor=backgroundColor,
                               ?frame=frame,
                               ?subviews=subviews)

        ViewElement.Create<UIKit.UIButton>(ViewBuilders.CreateUIButton, (fun prevOpt curr target -> ViewBuilders.UpdateUIButton(prevOpt, curr, target)), attribBuilder)

/// Viewer that allows to read the properties of a ViewElement representing a UIView
type UIViewViewer(element: ViewElement) =
    do if not ((typeof<UIKit.UIView>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'UIKit.UIView' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the BackgroundColor member
    member this.BackgroundColor = element.GetAttributeKeyed(ViewAttributes.UIViewBackgroundColorAttribKey)
    /// Get the value of the Frame member
    member this.Frame = element.GetAttributeKeyed(ViewAttributes.UIViewFrameAttribKey)
    /// Get the value of the Subviews member
    member this.Subviews = element.GetAttributeKeyed(ViewAttributes.UIViewSubviewsAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a UIViewController
type UIViewControllerViewer(element: ViewElement) =
    do if not ((typeof<UIKit.UIViewController>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'UIKit.UIViewController' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Title member
    member this.Title = element.GetAttributeKeyed(ViewAttributes.TitleAttribKey)
    /// Get the value of the View member
    member this.View = element.GetAttributeKeyed(ViewAttributes.UIViewControllerViewAttribKey)

/// Viewer that allows to read the properties of a ViewElement representing a UIStackView
type UIStackViewViewer(element: ViewElement) =
    inherit UIViewViewer(element)
    do if not ((typeof<UIKit.UIStackView>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'UIKit.UIStackView' is expected, but '%s' was provided." element.TargetType.FullName

/// Viewer that allows to read the properties of a ViewElement representing a UIButton
type UIButtonViewer(element: ViewElement) =
    inherit UIViewViewer(element)
    do if not ((typeof<UIKit.UIButton>).IsAssignableFrom(element.TargetType)) then failwithf "A ViewElement assignable to type 'UIKit.UIButton' is expected, but '%s' was provided." element.TargetType.FullName
    /// Get the value of the Title member
    member this.Title = element.GetAttributeKeyed(ViewAttributes.TitleAttribKey)
    /// Get the value of the TitleColor member
    member this.TitleColor = element.GetAttributeKeyed(ViewAttributes.UIButtonTitleColorAttribKey)

[<AbstractClass; Sealed>]
type View private () =
    /// Describes a UIView in the view
    static member inline UIView(?backgroundColor: UIKit.UIColor,
                                ?frame: CoreGraphics.CGRect,
                                ?subviews: ViewElement list) =

        ViewBuilders.ConstructUIView(?backgroundColor=backgroundColor,
                               ?frame=frame,
                               ?subviews=subviews)

    /// Describes a UIViewController in the view
    static member inline UIViewController(?title: string,
                                          ?view: ViewElement) =

        ViewBuilders.ConstructUIViewController(?title=title,
                               ?view=view)

    /// Describes a UIStackView in the view
    static member inline UIStackView(?backgroundColor: UIKit.UIColor,
                                     ?frame: CoreGraphics.CGRect,
                                     ?subviews: ViewElement list) =

        ViewBuilders.ConstructUIStackView(?backgroundColor=backgroundColor,
                               ?frame=frame,
                               ?subviews=subviews)

    /// Describes a UIButton in the view
    static member inline UIButton(?backgroundColor: UIKit.UIColor,
                                  ?frame: CoreGraphics.CGRect,
                                  ?subviews: ViewElement list,
                                  ?title: string,
                                  ?titleColor: UIKit.UIColor) =

        ViewBuilders.ConstructUIButton(?backgroundColor=backgroundColor,
                               ?frame=frame,
                               ?subviews=subviews,
                               ?title=title,
                               ?titleColor=titleColor)


[<AutoOpen>]
module ViewElementExtensions = 

    type ViewElement with

        /// Adjusts the UIViewBackgroundColor property in the visual element
        member x.UIViewBackgroundColor(value: UIKit.UIColor) = x.WithAttribute(ViewAttributes.UIViewBackgroundColorAttribKey, (value))

        /// Adjusts the UIViewFrame property in the visual element
        member x.UIViewFrame(value: CoreGraphics.CGRect) = x.WithAttribute(ViewAttributes.UIViewFrameAttribKey, (value))

        /// Adjusts the UIViewSubviews property in the visual element
        member x.UIViewSubviews(value: ViewElement list) = x.WithAttribute(ViewAttributes.UIViewSubviewsAttribKey, Array.ofList(value))

        /// Adjusts the Title property in the visual element
        member x.Title(value: string) = x.WithAttribute(ViewAttributes.TitleAttribKey, (value))

        /// Adjusts the UIViewControllerView property in the visual element
        member x.UIViewControllerView(value: ViewElement) = x.WithAttribute(ViewAttributes.UIViewControllerViewAttribKey, (value))

        /// Adjusts the UIButtonTitleColor property in the visual element
        member x.UIButtonTitleColor(value: UIKit.UIColor) = x.WithAttribute(ViewAttributes.UIButtonTitleColorAttribKey, (value))

        member inline x.With(?uIViewBackgroundColor: UIKit.UIColor, ?uIViewFrame: CoreGraphics.CGRect, ?uIViewSubviews: ViewElement list, ?title: string, ?uIViewControllerView: ViewElement, 
                             ?uIButtonTitleColor: UIKit.UIColor) =
            let x = match uIViewBackgroundColor with None -> x | Some opt -> x.UIViewBackgroundColor(opt)
            let x = match uIViewFrame with None -> x | Some opt -> x.UIViewFrame(opt)
            let x = match uIViewSubviews with None -> x | Some opt -> x.UIViewSubviews(opt)
            let x = match title with None -> x | Some opt -> x.Title(opt)
            let x = match uIViewControllerView with None -> x | Some opt -> x.UIViewControllerView(opt)
            let x = match uIButtonTitleColor with None -> x | Some opt -> x.UIButtonTitleColor(opt)
            x

    /// Adjusts the UIViewBackgroundColor property in the visual element
    let uIViewBackgroundColor (value: UIKit.UIColor) (x: ViewElement) = x.UIViewBackgroundColor(value)
    /// Adjusts the UIViewFrame property in the visual element
    let uIViewFrame (value: CoreGraphics.CGRect) (x: ViewElement) = x.UIViewFrame(value)
    /// Adjusts the UIViewSubviews property in the visual element
    let uIViewSubviews (value: ViewElement list) (x: ViewElement) = x.UIViewSubviews(value)
    /// Adjusts the Title property in the visual element
    let title (value: string) (x: ViewElement) = x.Title(value)
    /// Adjusts the UIViewControllerView property in the visual element
    let uIViewControllerView (value: ViewElement) (x: ViewElement) = x.UIViewControllerView(value)
    /// Adjusts the UIButtonTitleColor property in the visual element
    let uIButtonTitleColor (value: UIKit.UIColor) (x: ViewElement) = x.UIButtonTitleColor(value)
