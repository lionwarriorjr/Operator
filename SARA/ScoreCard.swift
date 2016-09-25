//
//  ScoreCard.swift
//  SARA
//
//  Created by Srihari Mohan on 9/24/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import AVFoundation
import UIKit

class ScoreCard: UIViewController {
    
    var speechSynthesizer = AVSpeechSynthesizer()
    var togglePlay = true
    
    @IBOutlet weak var composite: UILabel!
    @IBOutlet weak var targetDesc: UILabel!
    @IBOutlet weak var primary: UILabel!
    @IBOutlet weak var secondary: UILabel!
    @IBOutlet weak var tertiary: UILabel!
    @IBOutlet weak var summary: UITextView!
    @IBOutlet weak var mic: UIButton!
    @IBOutlet weak var scorecard: UIView!
    @IBOutlet weak var snapshot: UIImageView!

    override func viewDidLoad() {
        super.viewDidLoad()
        self.view.backgroundColor = UIColor.blackColor().colorWithAlphaComponent(0.7)
        scorecard.layer.borderWidth = 1
        scorecard.layer.borderColor = UIColor.clearColor().CGColor
        scorecard.layer.cornerRadius = 10
        scorecard.layer.masksToBounds = true
        PopUpViewController.showAnimate(self.view)
        mic.addTarget(self, action: #selector(ScoreCard.textToSpeech), forControlEvents: .TouchUpInside)
        summary.scrollEnabled = true
        summary.editable = false
        snapshot.layer.borderColor = UIColor.clearColor().CGColor
        snapshot.layer.cornerRadius = 8
        snapshot.layer.masksToBounds = true
    }
    
    func textToSpeech() {
        
        print("togglePlay: \(togglePlay)")
        
        if !togglePlay {
            print("paused")
            speechSynthesizer.pauseSpeakingAtBoundary(AVSpeechBoundary.Word)
        }
        
        if !speechSynthesizer.speaking {
            print("playing")
            let speechUtterance = AVSpeechUtterance(string: summary.text)
            speechUtterance.rate = 0.5
            speechUtterance.pitchMultiplier = 0.25
            speechUtterance.volume = 0.75
            speechSynthesizer.speakUtterance(speechUtterance)
        } else {
            speechSynthesizer.continueSpeaking()
        }
        
        togglePlay = !togglePlay
    }
    
    override func touchesBegan(touches: Set<UITouch>, withEvent event: UIEvent?) {
        PopUpViewController.removeAnimate(self.view)
    }
}
