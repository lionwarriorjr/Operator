//
//  SignInViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 9/23/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import Firebase
import UIKit

class SignInViewController: UIViewController, UITextFieldDelegate {
    
    @IBOutlet weak var navBar: UINavigationBar!
    @IBOutlet weak var name: UITextField!
    @IBOutlet weak var password: UITextField!
    @IBOutlet weak var signIn: UIButton!
    @IBOutlet weak var cancelButton: UIBarButtonItem!
    
    override func prefersStatusBarHidden() -> Bool {
        return true;
    }
    
    override func viewDidLoad() {
        let gradient: CAGradientLayer = CAGradientLayer()
        gradient.frame = self.view.bounds
        gradient.colors = [UIColor(netHex: 0xFFB75E).CGColor, UIColor(netHex: 0xED8F03).CGColor]
        self.view.layer.insertSublayer(gradient, atIndex: 0)
        navBar.titleTextAttributes = [NSForegroundColorAttributeName : UIColor(red: 50/255.0, green: 50/255.0, blue: 50/255.0, alpha: 1.0), NSFontAttributeName : UIFont(name: "Helvetica Neue", size: 16)!]
        name.delegate = self
        password.delegate = self
        name.backgroundColor = UIColor.whiteColor()
        password.backgroundColor = UIColor.whiteColor()
        name.borderStyle = .None
        password.borderStyle = .None
        name.layer.sublayerTransform = CATransform3DMakeTranslation(5, 0, 0)
        password.layer.sublayerTransform = CATransform3DMakeTranslation(5, 0, 0)
        name.attributedPlaceholder = NSAttributedString(string: "Email",
            attributes:[NSForegroundColorAttributeName: UIColor.grayColor()])
        password.attributedPlaceholder = NSAttributedString(string: "Password",
            attributes:[NSForegroundColorAttributeName: UIColor.grayColor()])
        signIn.layer.borderWidth = 1.0
        signIn.layer.borderColor = UIColor.whiteColor().CGColor
        signIn.addTarget(self, action: #selector(SignInViewController.auth), forControlEvents: .TouchUpInside)
        cancelButton.setTitleTextAttributes([ NSFontAttributeName: UIFont(name: "Helvetica Neue", size: 12)!], forState: UIControlState.Normal)
    }
    
    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)
        name.becomeFirstResponder()
    }
    
    override func viewWillDisappear(animated: Bool) {
        name.resignFirstResponder()
        password.resignFirstResponder()
        super.viewWillDisappear(animated)
    }
    
    @IBAction func dismissView(sender: AnyObject) {
        self.navigationController?.popViewControllerAnimated(false)
        let delay = 2.5 * Double(NSEC_PER_SEC)
        let time = dispatch_time(DISPATCH_TIME_NOW, Int64(delay))
        dispatch_after(time, dispatch_get_main_queue()) {}
    }
    
    func auth() {
        authenticate(name.text, password: password.text)
    }
    
    func authenticate(username: String?, password: String?) {
        
        if let username = username, password = password {
        
            FIRAuth.auth()?.signInWithEmail(username, password: password) { (user, error) in
                if error == nil {
                    //configure user settings
                    print("Signing In")
                
                    let feedVC = self.storyboard?.instantiateViewControllerWithIdentifier("FeedVC")
                        as! FeedViewController
                    self.navigationController?.pushViewController(feedVC, animated: true)
                
                } else {
                    let alertVC = UIAlertController(title: "Could Not Sign In", message: "Invalid username or password", preferredStyle: .Alert)
                    let alert_action = UIAlertAction(title: "OK", style: .Default, handler: nil)
                    alertVC.addAction(alert_action)
                    self.presentViewController(alertVC, animated: true, completion: nil)
                }
            }
        }
    }
}

extension SignInViewController {
    func textFieldShouldReturn(textField: UITextField) -> Bool {
        if textField == self.password {
            textField.resignFirstResponder()
            if let eText = name.text, pText = password.text {
                self.authenticate(eText, password: pText)
            }
        } else if textField == self.name {
            self.password.becomeFirstResponder()
        }
        return true;
    }
}
