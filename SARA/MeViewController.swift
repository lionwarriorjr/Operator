//
//  MeViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 8/21/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit

class MeViewController: UIViewController, UITableViewDelegate, UITableViewDataSource {
    
    var contents = MeViewController.populateSettings()
    var imgs = MeViewController.populateSettingsImg()
    
    @IBOutlet weak var playButton: UIBarButtonItem!
    @IBOutlet weak var profile: UIBarButtonItem!
    @IBOutlet weak var toolBar: UIToolbar!
    @IBOutlet weak var image: UIImageView!
    @IBOutlet weak var tableView: UITableView!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        let gradient: CAGradientLayer = CAGradientLayer()
        gradient.frame = self.view.bounds
        gradient.colors = [UIColor(netHex: 0xECE9E6).CGColor, UIColor(netHex: 0xeef2f3).CGColor]
        self.view.layer.insertSublayer(gradient, atIndex: 0)
        playButton.action = #selector(MeViewController.displayFeed)
        self.view.clipsToBounds = true
        toolBar.barTintColor = UIColor(red: 210/255.0, green: 210/255.0, blue: 210/255.0, alpha: 1)
        image.layer.borderWidth = 1
        image.layer.masksToBounds = false
        image.layer.borderColor = UIColor.whiteColor().CGColor
        image.layer.cornerRadius = image.frame.height/2
        image.clipsToBounds = true
        tableView.backgroundColor = UIColor.clearColor()
        tableView.scrollEnabled = false
    }
    
    func displayFeed() {
        playButton.tintColor = UIColor(red: 0, green: 122/255.0, blue: 250/255.0, alpha: 1)
        profile.tintColor = UIColor(red: 50/255.0, green: 50/255.0, blue: 50/255.0, alpha: 1)
        let feedVC = self.storyboard?.instantiateViewControllerWithIdentifier("FeedVC") as! FeedViewController
        self.navigationController?.pushViewController(feedVC, animated: false)
    }
    
    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)
        profile.tintColor = UIColor(red: 0, green: 122/255.0, blue: 250/255.0, alpha: 1)
        playButton.tintColor = UIColor(red: 50/255.0, green: 50/255.0, blue: 50/255.0, alpha: 1)
    }
    
    func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return contents.count;
    }
    
    func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        
        let cell = tableView.dequeueReusableCellWithIdentifier("SettingsCell")! as UITableViewCell
        cell.backgroundColor = UIColor.clearColor()
        
        if indexPath.row % 2 == 0 {
            cell.textLabel?.text = contents[indexPath.row]
            cell.imageView?.image = imgs[indexPath.row/2]
        } else {
            cell.userInteractionEnabled = false
        }
        
        return cell
    }
    
    func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        
        if tableView.cellForRowAtIndexPath(indexPath)!.textLabel?.text
            == "Phone" {
            let phoneVC = self.storyboard?.instantiateViewControllerWithIdentifier("PhoneVC")
                as! PhoneViewController
            self.presentViewController(phoneVC, animated: true, completion: nil)
        } else if tableView.cellForRowAtIndexPath(indexPath)!.textLabel?.text
            == "Privacy & Terms" {
            let privacyVC = self.storyboard?.instantiateViewControllerWithIdentifier("StandardTableVC")
                as! StandardTableViewController
            privacyVC.sects = ["Our Data Policy", "FDA and HIPAA Compliance, Data Sharing"]
            privacyVC.items = [["Data Policy", "Terms of Service", "Third Party Integration"], ["FDA Classification", "HIPAA Compliance",
                "HIPAA Security Rule","Integration With EHR", "Integration with Hospital System"], ["Data Sharing Among Provider Ecosystem", "Liabilities and Malpractice Reimbursements"]]
            self.navigationController?.pushViewController(privacyVC, animated: true)
        } else if tableView.cellForRowAtIndexPath(indexPath)!.textLabel?.text
            == "Report a Problem" {
            let alertVC = UIAlertController(title: "What about your experience can be improved?", message: "Please let us know at operator.com.", preferredStyle: .Alert)
            let alert_action = UIAlertAction(title: "Cancel", style: .Default, handler: nil)
            alertVC.addAction(alert_action)
            self.presentViewController(alertVC, animated: true, completion: nil)
        }
        
        tableView.deselectRowAtIndexPath(indexPath, animated: true)
    }
    
    private static func populateSettings() -> [String] {
        var contents = [String]()
        contents.appendContentsOf(["Phone", "", "Privacy & Terms", "", "Report a Problem"])
        return contents;
    }
    
    private static func populateSettingsImg() -> [UIImage] {
        var imgs = [UIImage]()
        imgs.appendContentsOf([UIImage(named:"Phone")!, UIImage(named: "Privacy")!, UIImage(named: "Help")!])
        return imgs;
    }
}
