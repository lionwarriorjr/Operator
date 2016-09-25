//
//  FeedViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 9/24/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import GoogleSignIn
import Firebase
import UIKit

class FeedViewController: UIViewController, UICollectionViewDelegate, UICollectionViewDataSource, UICollectionViewDelegateFlowLayout,
    UISearchBarDelegate {
    
    @IBOutlet weak var collectionView: UICollectionView!
    @IBOutlet weak var navBar: UINavigationBar!
    @IBOutlet weak var searchBar: UISearchBar!
    @IBOutlet weak var playButton: UIBarButtonItem!
    @IBOutlet weak var toolBar: UIToolbar!
    
    let surgeries:[Surgery] = Surgery.testSet
    var searchActive: Bool = false
    var filtered:[Surgery] = []
    var colors = [UIColor]()
    let btn = UIButton()
    var appDelegate = UIApplication.sharedApplication().delegate as! AppDelegate
    
    override func viewDidLoad() {
        super.viewDidLoad()
        for _ in 0...(surgeries.count-1) {
            colors.append(randomColor(luminosity: .Dark))
        }
        self.view.clipsToBounds = true
        navBar.barTintColor = UIColor(netHex: 0x29323c)
        searchBar.delegate = self
        searchBar.returnKeyType = .Done
        searchBar.backgroundImage = UIImage()
        btn.setTitle("Dismiss", forState: .Normal)
        btn.setTitleColor(UIColor(red: 236/255.0, green: 180/255.0, blue: 91/255.0, alpha: 1), forState: .Normal)
        btn.frame = CGRectMake(280, 300, 100, 100)
        btn.alpha = 1
        btn.addTarget(self, action: #selector(FeedViewController.dismissKeyboard), forControlEvents: .TouchUpInside)
        navBar.tintColor = UIColor(red: 50/255.0, green: 50/255.0, blue: 50/255.0, alpha: 1)
        navBar.titleTextAttributes = [NSForegroundColorAttributeName : UIColor(red: 255/255.0, green: 255/255.0, blue: 255/255.0, alpha: 1.0), NSFontAttributeName : UIFont(name: "Avenir Next", size: 18)!]
        
        var searchBarTextField : UITextField!
        for view1 in searchBar.subviews {
            for view2 in view1.subviews {
                if view2.isKindOfClass(UITextField) {
                    searchBarTextField = view2 as! UITextField
                    searchBarTextField.enablesReturnKeyAutomatically = false
                    break;
                }
            }
        }
        
        toolBar.barTintColor = UIColor(red: 210/255.0, green: 210/255.0, blue: 210/255.0, alpha: 1)
    }
    
    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)
        self.subscribeToKeyboardNotifications()
        self.subscribeToKeyboardHidden()
        playButton.tintColor = UIColor(red: 0/255.0, green: 122/255.0, blue: 250/255.0, alpha: 1)
        //profile.tintColor = UIColor(red: 50/255.0, green: 50/255.0, blue: 50/255.0, alpha: 1)
    }
    
    override func viewWillDisappear(animated: Bool) {
        super.viewWillDisappear(animated)
        self.unsubscribeFromKeyboardNotifications()
        self.unsubscribeFromKeyboardHidden()
    }
    
    override func prefersStatusBarHidden() -> Bool {
        return true;
    }
    
    @IBAction func refresh(sender: AnyObject) {
        searchActive = false
        filtered = []
        collectionView.reloadData()
    }
    
    @IBAction func logout(sender: AnyObject) {
        let alertVC = UIAlertController(title: "Are you sure you want to log out?", message: "", preferredStyle: .Alert)
        var alert_action = UIAlertAction(title: "Log Out", style: .Default) {
            alert in
            do {
                self.navigationController?.popToRootViewControllerAnimated(true)
                try FIRAuth.auth()!.signOut()
                if GIDSignIn.sharedInstance().currentUser != nil {
                    GIDSignIn.sharedInstance().signOut()
                }
            } catch {
                print("Error in Log Out")
            }
        }
        alertVC.addAction(alert_action)
        alert_action = UIAlertAction(title: "Cancel", style: .Default, handler: nil)
        alertVC.addAction(alert_action)
        self.presentViewController(alertVC, animated: true, completion: nil)
    }
    
    func collectionView(collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, sizeForItemAtIndexPath indexPath: NSIndexPath) -> CGSize {
        return CGSize(width: UIScreen.mainScreen().bounds.width, height: UIScreen.mainScreen().bounds.height/3)
    }
    
    func collectionView(collectionView: UICollectionView, layout collectionViewLayout: UICollectionViewLayout, minimumLineSpacingForSectionAtIndex section: Int) -> CGFloat {
        return 0
    }
    
    func collectionView(collectionView: UICollectionView, numberOfItemsInSection section: Int) -> Int {
        if searchActive {
            return filtered.count;
        }
        return surgeries.count;
    }
    
    func collectionView(collectionView: UICollectionView, cellForItemAtIndexPath indexPath: NSIndexPath) -> UICollectionViewCell {
        
        let cell = collectionView.dequeueReusableCellWithReuseIdentifier("procedure", forIndexPath: indexPath) as! ProcedureCell
        let surgery = (searchActive) ? filtered[indexPath.item]
            : self.surgeries[indexPath.item]
        let imageView = UIImageView(image: surgery.surgeonImage)
        imageView.contentMode = UIViewContentMode.ScaleToFill
        cell.backgroundView = imageView
        cell.procedureLabel?.text = surgery.procedure
        cell.surgeonLabel?.text = surgery.surgeon
        cell.liveStatus?.text = (surgery.liveStatus) ? "Live" : ""
        cell.colored?.tintColor = colors[indexPath.item]
        cell.genre?.text = surgery.genre
        
        if surgery.liveStatus {
            cell.liveStatus?.textColor = UIColor(netHex: 0xef473a)
        }
        
        cell.backgroundView?.alpha = 0.75
        cell.backgroundView?.tintColor = UIColor.blackColor()
        return cell;
    }
    
    func collectionView(collectionView: UICollectionView, didSelectItemAtIndexPath indexPath: NSIndexPath) {
        let sessionVC = self.storyboard?.instantiateViewControllerWithIdentifier("SessionVC")
            as! SessionViewController
        sessionVC.navTitle = surgeries[indexPath.item].procedure
        appDelegate.videoFeed = surgeries[indexPath.item].videoFeed
        appDelegate.selectedRisks = surgeries[indexPath.item].risks
        self.navigationController?.pushViewController(sessionVC, animated: true)
    }
}

extension FeedViewController {
    
    func searchBarSearchButtonClicked(searchBar: UISearchBar) {
        searchActive = false
        searchBar.resignFirstResponder()
    }
    
    func searchBarTextDidBeginEditing(searchBar: UISearchBar) {
        searchActive = true;
    }
    
    func searchBarTextDidEndEditing(searchBar: UISearchBar) {
        searchActive = false;
    }
    
    func searchBarCancelButtonClicked(searchBar: UISearchBar) {
        searchActive = false;
    }
    
    func searchBar(searchBar: UISearchBar, textDidChange searchText: String) {
        
        filtered = surgeries.filter({ (current) -> Bool in
            let tmp: Surgery = current
            return tmp.surgeon.rangeOfString(searchText, options: NSStringCompareOptions.CaseInsensitiveSearch) != nil
        })
        
        if(filtered.count == 0){
            searchActive = false;
        } else {
            searchActive = true;
        }
        
        collectionView.reloadData()
    }
}

extension FeedViewController {
    
    func dismissKeyboard() {
        searchBar.resignFirstResponder()
    }
    
    func keyboardWillShow(notification: NSNotification) {
        //collectionView.addSubview(btn)
        //collectionView.bringSubviewToFront(btn)
    }
    
    func keyboardWillHide(notification: NSNotification) {
        //btn.removeFromSuperview()
    }
    
    func getKeyboardHeight(notification: NSNotification) -> CGFloat {
        //userInfo dictionary holds user information like the size of the keyboard
        let userInfo = notification.userInfo
        let keyboardSize = userInfo![UIKeyboardFrameEndUserInfoKey] as! NSValue
        return keyboardSize.CGRectValue().height
    }
    
    func subscribeToKeyboardHidden() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(keyboardWillHide), name: UIKeyboardWillHideNotification, object: nil)
    }
    
    func subscribeToKeyboardNotifications() {
        //notification of when UIKeyboardWillShow
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(keyboardWillShow), name: UIKeyboardDidShowNotification, object: nil)
    }
    
    func unsubscribeFromKeyboardNotifications() {
        NSNotificationCenter.defaultCenter().removeObserver(self, name:
            UIKeyboardWillShowNotification, object: nil)
    }
    
    func unsubscribeFromKeyboardHidden() {
        NSNotificationCenter.defaultCenter().removeObserver(self, name: UIKeyboardWillHideNotification, object: nil)
    }
}
