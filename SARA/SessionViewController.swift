//
//  SessionViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 9/23/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit
import AVKit
import AVFoundation
import CoreGraphics

class SessionViewController: UIViewController, SWRevealViewControllerDelegate, SessionViewDelegate {
    
    @IBOutlet weak var contextView: SessionView!
    @IBOutlet weak var navBar: UINavigationBar!
    @IBOutlet weak var leave: UIButton!
    @IBOutlet weak var tools: UIBarButtonItem!
    @IBOutlet weak var risk: UIBarButtonItem!
    @IBOutlet weak var riskLabel: UILabel!
    @IBOutlet weak var score: UIButton!
    @IBOutlet weak var divider: UIView!
    
    var navTitle: String!
    var player = AVPlayer()
    var playerLayer = AVPlayerLayer()
    let appDelegate = UIApplication.sharedApplication().delegate as! AppDelegate
    
    override func prefersStatusBarHidden() -> Bool {
        return true;
    }
    
    override func viewDidAppear(animated: Bool) {
        super.viewDidAppear(animated)
        let currentRisk = appDelegate.currentRisk
        riskLabel.text = (currentRisk == "") ? ""
            : "Risk: " + currentRisk
        riskLabel.textColor = UIColor(red: 1, green: 104/255.0, blue: 102/255.0, alpha: 1)
        score.setTitle("", forState: .Normal)
        appDelegate.currentTool = "Grasping Forceps"
        appDelegate.currentColor = UIColor(netHex: 0x1ca0de)
        appDelegate.player = self.player
    }
    
    override func viewDidLoad() {
        super.viewDidLoad()
        contextView.delegate = self
        self.revealViewController().delegate = self
        self.navigationController?.navigationBarHidden = true
        navBar.opaque = true
        navBar.topItem?.title = navTitle
        navBar.titleTextAttributes = [NSForegroundColorAttributeName : UIColor(red: 255/255.0, green: 255/255.0, blue: 255/255.0, alpha: 1.0), NSFontAttributeName : UIFont(name: "Helvetica Neue", size: 17)!]
        leave.addTarget(self, action: #selector(SessionViewController.dismissView), forControlEvents: .TouchUpInside)
        risk.target = self.revealViewController()
        risk.action = #selector(SWRevealViewController.rightRevealToggle(_:))
        tools.target = self.revealViewController()
        tools.action = #selector(SWRevealViewController.revealToggle(_:))
        let path = NSBundle.mainBundle().pathForResource(appDelegate.videoFeed, ofType: "mp4")
        print(path)
        let fileURL = NSURL(fileURLWithPath: path!)
        player = AVPlayer(URL: fileURL)
        playerLayer = AVPlayerLayer(player: player)
        playerLayer.frame = self.view.frame
        if appDelegate.videoFeed == "Capsular" {
            playerLayer.videoGravity = AVLayerVideoGravityResizeAspectFill
        } else {
            playerLayer.videoGravity = AVLayerVideoGravityResize
        }
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(SessionViewController.playerItemDidReachEnd), name: AVPlayerItemDidPlayToEndTimeNotification, object: player.currentItem)
        self.contextView.backgroundColor = UIColor.clearColor()
        self.view.layer.addSublayer(playerLayer)
        self.view.bringSubviewToFront(contextView)
        self.view.bringSubviewToFront(navBar)
        self.view.bringSubviewToFront(leave)
        self.view.bringSubviewToFront(score)
        self.view.bringSubviewToFront(divider)
        score.titleLabel?.adjustsFontSizeToFitWidth = true
        riskLabel.adjustsFontSizeToFitWidth = true
        score.addTarget(self, action: #selector(SessionViewController.showScorecard), forControlEvents: .TouchUpInside)
        player.seekToTime(kCMTimeZero)
        player.play()
    }
    
    func showScorecard() {
        let scorecardVC = self.storyboard?.instantiateViewControllerWithIdentifier("ScoreCardVC")
            as! ScoreCard
        self.addChildViewController(scorecardVC)
        scorecardVC.view.frame = self.view.frame
        self.view.addSubview(scorecardVC.view)
        scorecardVC.didMoveToParentViewController(self)
        scorecardVC.composite.text = "\(appDelegate.currentScore)"
        scorecardVC.targetDesc.text = appDelegate.currentRisk
        scorecardVC.primary.text = appDelegate.primaryText
        scorecardVC.secondary.text = appDelegate.secondaryText
        scorecardVC.tertiary.text = appDelegate.tertiaryText
        scorecardVC.summary.text = appDelegate.summaryText
        scorecardVC.snapshot.image = appDelegate.snapshot
    }
    
    func makeDynamicChanges() {
        score.setTitle("Risk Score: " + "\(appDelegate.currentScore)",
            forState: .Normal)
        score.titleLabel?.font = UIFont(name: "Helvetica-Bold", size: 20)
        switch (appDelegate.currentScore) {
        case 0...35: score.setTitleColor(UIColor(red: 30/255.0, green: 215/255.0, blue: 96/255.0, alpha: 1), forState: .Normal)
        case 36...65: score.setTitleColor(UIColor(red: 240/255.0, green: 184/255.0, blue: 35/255.0, alpha: 1), forState: .Normal)
        default: score.setTitleColor(UIColor(red: 255/255.0, green: 102/255.0, blue: 104/255.0, alpha: 1), forState: .Normal)
        }
    }
    
    static func videoSnapshot() -> UIImage? {
        
        let path = NSBundle.mainBundle().pathForResource((UIApplication.sharedApplication().delegate as! AppDelegate).videoFeed, ofType: "mp4")
        let fileURL = NSURL(fileURLWithPath: path!)
        let asset = AVURLAsset(URL: fileURL)
        let generator = AVAssetImageGenerator(asset: asset)
        generator.appliesPreferredTrackTransform = true
    
        let currentTime = CMTimeGetSeconds(((UIApplication.sharedApplication().delegate as! AppDelegate).player.currentItem?.currentTime())!)
        let timestamp = CMTime(seconds: currentTime, preferredTimescale: 60)
    
        do {
            let imageRef = try generator.copyCGImageAtTime(timestamp, actualTime: nil)
            return UIImage(CGImage: imageRef)
        }
        catch let error as NSError
        {
            print("Image generation failed with error \(error)")
            return nil
        }
    }
    
    func revealController(revealController: SWRevealViewController!, willMoveToPosition position: FrontViewPosition) {
        
        let rightVC = revealController.rightViewController as? UINavigationController
        
        if rightVC == nil {
            return;
        }
        
        if ((rightVC?.topViewController?.isKindOfClass(RiskViewController)) != nil) {
            let riskVC = rightVC?.topViewController as! RiskViewController
            riskVC.risks = appDelegate.selectedRisks
        }
        
        let currentRisk = appDelegate.currentRisk
        riskLabel.text = (currentRisk == "") ? "" : "Risk: " + currentRisk
        contextView.currentColor = appDelegate.currentColor
    }
    
    func playerItemDidReachEnd(notification: NSNotification) {
        self.player.seekToTime(kCMTimeZero)
        player.play()
    }
    
    func dismissView() {
        self.navigationController?.popViewControllerAnimated(true)
    }
}
