//
//  PopUpViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 9/24/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit

class PopUpViewController: UIViewController, UITextViewDelegate {
    
    @IBOutlet weak var popUpView: UIView!
    @IBOutlet weak var eventTextView: UITextView!
    @IBOutlet weak var descTextView: UITextView!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.view.backgroundColor = UIColor.blackColor().colorWithAlphaComponent(0.7)
        popUpView.layer.borderWidth = 1
        popUpView.layer.borderColor = UIColor.clearColor().CGColor
        popUpView.layer.cornerRadius = 10
        popUpView.layer.masksToBounds = true
        eventTextView.textColor = UIColor.lightGrayColor()
        descTextView.textColor = UIColor.lightGrayColor()
        PopUpViewController.showAnimate(self.view)
        let tapper = UITapGestureRecognizer(target: view, action:#selector(UIView.endEditing))
        tapper.cancelsTouchesInView = false
        view.addGestureRecognizer(tapper)
    }
    
    @IBAction func save(sender: AnyObject) {
    
    }
    
    @IBAction func closePopUp(sender: AnyObject) {
        PopUpViewController.removeAnimate(self.view)
    }
    
    func textViewDidEndEditing(textView: UITextView) {
        if textView.text.isEmpty {
            textView.text = (textView == eventTextView) ? "Event" : "Description"
            textView.textColor = UIColor.lightGrayColor()
        }
    }
    
    func textViewDidBeginEditing(textView: UITextView) {
        if textView.textColor == UIColor.lightGrayColor() {
            textView.text = ""
            textView.textColor = UIColor.blackColor()
        }
    }
    
    static func showAnimate(view: UIView)
    {
        view.transform = CGAffineTransformMakeScale(1.3, 1.3)
        view.alpha = 0.0;
        UIView.animateWithDuration(0.25, animations: {
            view.alpha = 1.0
            view.transform = CGAffineTransformMakeScale(1.0, 1.0)
        });
    }
    
    static func removeAnimate(view: UIView)
    {
        UIView.animateWithDuration(0.25, animations: {
            view.transform = CGAffineTransformMakeScale(1.3, 1.3)
            view.alpha = 0.0;
            }, completion:{(finished : Bool)  in
                if (finished)
                {
                    view.removeFromSuperview()
                }
        });
    }
}
