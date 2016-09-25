//
//  LoginViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 9/23/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import Google
import GoogleSignIn
import GGLSignIn
import UIKit
import AVFoundation
import AVKit

class LoginViewController: UIViewController, GIDSignInUIDelegate, GIDSignInDelegate {
    
    var player = AVPlayer()
    var playerLayer = AVPlayerLayer()
    var appDelegate = UIApplication.sharedApplication().delegate as! AppDelegate
    
    @IBOutlet weak var signIn: UIButton!
    @IBOutlet weak var register: UIButton!
    @IBOutlet weak var operatorTitle: UILabel!
    @IBOutlet weak var googleSignIn: GIDSignInButton!
    
    override func prefersStatusBarHidden() -> Bool {
        return true;
    }
    
    override func viewDidLoad() {
        
        super.viewDidLoad()
        GIDSignIn.sharedInstance().uiDelegate = self
        GIDSignIn.sharedInstance().delegate = self
        appDelegate.setGradient(operatorTitle, color1: UIColor(red: 255/255.0, green: 169/255.0, blue: 0/255.0, alpha: 1),
            color2: UIColor(netHex: 0xED8F03))
        let path = NSBundle.mainBundle().pathForResource("operatorHome", ofType: "mp4")
        let fileURL = NSURL(fileURLWithPath: path!)
        player = AVPlayer(URL: fileURL)
        playerLayer = AVPlayerLayer(player: player)
        playerLayer.frame = self.view.frame
        playerLayer.videoGravity = AVLayerVideoGravityResizeAspectFill
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(LoginViewController.playerItemDidReachEnd(_:)), name: "continueVideo", object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(LoginViewController.playerItemDidReachEnd), name: AVPlayerItemDidPlayToEndTimeNotification, object: player.currentItem)
        playerLayer.opacity = 0.9
        self.view.layer.addSublayer(playerLayer)
        self.view.bringSubviewToFront(operatorTitle)
        self.view.bringSubviewToFront(signIn)
        self.view.bringSubviewToFront(register)
        self.view.bringSubviewToFront(googleSignIn)
        operatorTitle.textColor = UIColor.whiteColor()
        operatorTitle.alpha = 1
        signIn.alpha = 1
        register.alpha = 1
        appDelegate.setGradient(register, color1: UIColor(red: 255/255.0, green: 169/255.0, blue: 0/255.0, alpha: 1), color2: UIColor(netHex: 0xED8F03))
        appDelegate.setGradient(signIn, color1: UIColor(netHex: 0x485563), color2: UIColor(netHex: 0x29323c))
        let horizontalMotionEffect = UIInterpolatingMotionEffect(keyPath: "center.x", type: .TiltAlongHorizontalAxis)
        horizontalMotionEffect.minimumRelativeValue = -5
        horizontalMotionEffect.maximumRelativeValue = 5
        let group = UIMotionEffectGroup()
        group.motionEffects = [horizontalMotionEffect]
        self.view.addMotionEffect(group)
        operatorTitle.addMotionEffect(group)
        signIn.addMotionEffect(group)
        register.addMotionEffect(group)
        googleSignIn.addMotionEffect(group)
    }
    
    func signIn(signIn: GIDSignIn!, didSignInForUser user: GIDGoogleUser!, withError error: NSError!) {
        if error != nil {
            print(error)
        } else {
            print(user.profile.email)
            self.performSegueWithIdentifier("googleSignIn", sender: self)
        }
    }
    
    override func viewWillAppear(animated: Bool) {
        player.seekToTime(kCMTimeZero)
        player.play()
        super.viewWillAppear(animated)
    }
    
    func playerItemDidReachEnd(notification: NSNotification) {
        self.player.seekToTime(kCMTimeZero)
        player.play()
    }
    
    func applicationWillEnterForeground(application: UIApplication) {
        NSNotificationCenter.defaultCenter().postNotificationName("continueVideo", object: nil)
    }
    
    @IBAction func verify(sender: AnyObject) {
        let signinVC = self.storyboard?.instantiateViewControllerWithIdentifier("SignInVC") as!
        SignInViewController
        self.navigationController?.pushViewController(signinVC, animated: false)
    }
}
