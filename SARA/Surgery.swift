//
//  Surgery.swift
//  SARA
//
//  Created by Srihari Mohan on 9/23/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit

struct Surgery {
    var genre: String
    var surgeon: String
    var procedure: String
    var liveStatus: Bool
    var surgeonImage: UIImage?
    var videoFeed: String
    var risks: [String]
    
    static let GenreKey = "GenreKey"
    static let SurgeonKey = "SurgeonKey"
    static let ProcedureKey = "ProcedureKey"
    static let StatusKey = "StatusKey"
    static let ImageKey = "ImageKey"
    static let VideoKey = "VideoKey"
    static let RisksKey = "RisksKey"
    
    init(dictionary: [String: AnyObject?]) {
        self.genre = dictionary[Surgery.GenreKey] as! String
        self.surgeon = dictionary[Surgery.SurgeonKey] as! String
        self.procedure = dictionary[Surgery.ProcedureKey] as! String
        self.liveStatus = dictionary[Surgery.StatusKey] as! Bool
        self.surgeonImage = dictionary[Surgery.ImageKey] as! UIImage?
        self.videoFeed = dictionary[Surgery.VideoKey] as! String
        self.risks = dictionary[Surgery.RisksKey] as! [String]
    }
}

extension Surgery {
    static var testSet: [Surgery] {
        get {
            var testInbox = [Surgery]()
            for d in Surgery.localProcedures() {
                testInbox.append(Surgery(dictionary: d))
            }
            return testInbox;
        }
    }
    
    static func localProcedures() -> [[String: AnyObject?]] {
        var demoDictionary: [[String: AnyObject?]]
        demoDictionary = [
            [Surgery.GenreKey: "opthamology", Surgery.SurgeonKey: "Surgeon ID 1", Surgery.ProcedureKey: "Cataract Removal", Surgery.StatusKey: true, Surgery.ImageKey: UIImage(named: "TestDoc1"),
                Surgery.VideoKey: "Capsular", Surgery.RisksKey: ["posterior capsular rupture", "runaway tear", "cystoid macular edema", "corneal edema", "hyphema", "retinal detachment"]],
            [Surgery.GenreKey: "gynecology", Surgery.SurgeonKey: "Surgeon ID 2", Surgery.ProcedureKey: "Diagnostic Laparoscopy", Surgery.StatusKey: false, Surgery.ImageKey: UIImage(named: "TestDoc2"), Surgery.VideoKey: "SurgicalTraining", Surgery.RisksKey: ["Organ Perforation", "Tissue Perforation","Internal Hemorrhage", "Blood Clots", "Scarring", "Nerve Damage", "Swelling", "Postoperative Infection", "Bruising"]],
            [Surgery.GenreKey: "orthopedics", Surgery.SurgeonKey: "Surgeon ID 3", Surgery.ProcedureKey: "Kidney Biopsy", Surgery.StatusKey: false, Surgery.ImageKey: UIImage(named: "TestDoc3"), Surgery.VideoKey: "Intuitive", Surgery.RisksKey: ["Organ Perforation", "Tissue Perforation","Internal Hemorrhage", "Blood Clots", "Scarring", "Nerve Damage", "Swelling", "Postoperative Infection", "Bruising"]],
            [Surgery.GenreKey: "obstetrics", Surgery.SurgeonKey: "Surgeon ID 4", Surgery.ProcedureKey: "Cesarian Section", Surgery.StatusKey: false, Surgery.ImageKey: UIImage(named: "TestDoc4"), Surgery.VideoKey: "Intuitive", Surgery.RisksKey: ["Organ Perforation", "Tissue Perforation","Internal Hemorrhage", "Blood Clots", "Scarring", "Nerve Damage", "Swelling", "Postoperative Infection", "Bruising"]],
            [Surgery.GenreKey: "general surgery", Surgery.SurgeonKey: "Surgeon ID 5", Surgery.ProcedureKey: "Septic Shock", Surgery.StatusKey: false, Surgery.ImageKey: UIImage(named: "TestDoc5"), Surgery.VideoKey: "Intuitive", Surgery.RisksKey: ["Organ Perforation", "Tissue Perforation","Internal Hemorrhage", "Blood Clots", "Scarring", "Nerve Damage", "Swelling", "Postoperative Infection", "Bruising"]]
        ]
        return demoDictionary;
    }
}