namespace Fabulous.iOS
open Fabulous

module ViewUpdaters =    
    let updateUIViewSubviews (prevOpt: ViewElement array voption) (currOpt: ViewElement array voption) (target: UIKit.UIView) _ =
        // TODO: Implement reuse mechanism
        for subview in target.Subviews do
            subview.RemoveFromSuperview()
            
        match currOpt with
        | ValueNone -> ()
        | ValueSome curr ->
            for element in curr do
                target.AddSubview (element.Create() :?> UIKit.UIView)
    
    let updateUIButtonTitle (prevOpt: string voption) (currOpt: string voption) (target: UIKit.UIButton) =
        match prevOpt, currOpt with
        | ValueNone, ValueNone -> ()
        | ValueSome prev, ValueSome curr when prev = curr -> ()
        | _, ValueSome curr -> target.SetTitle(curr, UIKit.UIControlState.Normal)
        | ValueSome _, _ -> target.SetTitle(null, UIKit.UIControlState.Normal)
    
    let updateUIButtonTitleColor (prevOpt: UIKit.UIColor voption) (currOpt: UIKit.UIColor voption) (target: UIKit.UIButton) =
        match prevOpt, currOpt with
        | ValueNone, ValueNone -> ()
        | ValueSome prev, ValueSome curr when prev = curr -> ()
        | _, ValueSome curr ->
            target.SetTitleColor(curr, UIKit.UIControlState.Normal)
        | ValueSome _, _ ->
            target.SetTitleColor(null, UIKit.UIControlState.Normal)
