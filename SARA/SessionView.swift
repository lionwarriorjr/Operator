//
//  SessionView.swift
//  SARA
//
//  Created by Srihari Mohan on 9/23/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit
import CoreGraphics

func getDocumentsURL() -> NSURL {
    let documentsURL = NSFileManager.defaultManager().URLsForDirectory(.DocumentDirectory, inDomains: .UserDomainMask)[0]
    return documentsURL
}

func fileInDocumentsDirectory(filename: String) -> String {
    let fileURL = getDocumentsURL().URLByAppendingPathComponent(filename)
    return fileURL.path!
}

class SessionView: UIView {
    
    var delegate: SessionViewDelegate?
    
    var moves = [Move]()
    var snapshot = [CGPoint]()
    var lastPoint: CGPoint!
    var currentColor = UIColor(netHex: 0x1ca0de)
    var minX: CGFloat?
    var minY: CGFloat?
    var maxX: CGFloat?
    var maxY: CGFloat?
    
    required init(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)!
    }
    
    override func touchesBegan(touches: Set<UITouch>, withEvent event: UIEvent?) {
        minX = super.frame.width
        minY = super.frame.height
        maxX = 0
        maxY = 0
        lastPoint = touches.first?.locationInView(self)
        updateBounds()
    }
    
    func updateBounds() {
        if lastPoint.x >= maxX {
            maxX = lastPoint.x
        }
        if lastPoint.x < minX {
            minX = lastPoint.x
        }
        if lastPoint.y >= maxY {
            maxY = lastPoint.y
        }
        if lastPoint.y < minY {
            minY = lastPoint.y
        }
    }
    
    override func touchesMoved(touches: Set<UITouch>, withEvent event: UIEvent?) {
        if lastPoint != nil {
            let newPoint = touches.first?.locationInView(self)
            if let newPoint = newPoint {
                let move = Move(start: lastPoint, end: newPoint)
                moves.append(move)
                lastPoint = newPoint
                updateBounds()
            }
            self.setNeedsDisplay()
        }
    }
    
    override func touchesEnded(touches: Set<UITouch>, withEvent event: UIEvent?) {
        moves = []
        snapshot = []
        self.setNeedsDisplay()
    }
    
    override func drawRect(rect: CGRect) {
        
        if moves.count == 1 { return; }
        
        snapshot = []
        
        let context = UIGraphicsGetCurrentContext()
        CGContextSetLineCap(context, CGLineCap.Round)
        CGContextSetLineWidth(context, 4.5)
        
        for move in moves {
            CGContextBeginPath(context)
            CGContextMoveToPoint(context, move.start.x, move.start.y)
            CGContextAddLineToPoint(context, move.end.x, move.end.y)
            CGContextSetStrokeColorWithColor(context, currentColor.CGColor)
            CGContextStrokePath(context)
        }
        
        if let maxX = maxX, maxY = maxY, minX = minX, minY = minY {
            
            if maxX - minX == 0 || maxY - minY == 0 { return; }
            
            let bottomLeft = CGPoint(x: minX, y: minY)
            let topLeft = CGPoint(x: minX, y: maxY)
            let bottomRight = CGPoint(x: maxX, y: minY)
            let topRight = CGPoint(x: maxX, y: maxY)
            self.snapshot.appendContentsOf([bottomLeft, topLeft, topRight, bottomRight])
            self.takeSnapshot()
            
            let image = self.takeImage(maxX, minX: minX, maxY: maxY, minY: minY)
            let base64String = base64EncodeImage(image)
            let imageURL = NSURL(string: base64String)
            var redComp: Double = 1.0
            var greenComp: Double = 1.0
            var blueComp: Double = 1.0
            
            if imageURL == nil {
                return;
            }
            
            var kinematicsScore = 0.0
            var imageScore = 0.0
            
            let x = generateKinematics()
            OperatorClient.taskForR(x) { (results, error) in
                if error == nil {
                    print("R Analysis")
                    if let scores = results["result"] as? [Double] {
                        let closureVar: () = {() -> () in
                            kinematicsScore = scores[0]
                            var imagePath = fileInDocumentsDirectory("operator_snapshot.png")
                            self.saveImage(image, path: imagePath)
                            imagePath = "@" + imagePath
                            OperatorClient.taskForWatsonVisionMethod(imagePath) { (results, error) in
                                print(results)
                            }
                            imageScore = kinematicsScore
                        }()
                        closureVar
                    }
                }
            }
            
            OperatorClient.taskForPOSTVisionMethod(base64String) { (results, error) in

                if let error = error {
                    print(error)
                } else {

                    print("Client Handling Vision API Call")
                    print(results)
                    if let responses = results[OperatorClient.JSONResponseKeys.Responses] as? [[String: AnyObject]] {
                        print("in responses")
                        
                        if let imageProperties = responses[0][OperatorClient.JSONResponseKeys.ImagePropertiesAnnotation] as? [String: AnyObject] {
                            print("in imageProperties")
                            
                            if let dominantColors = imageProperties[OperatorClient.JSONResponseKeys.DominantColors] as? [String: AnyObject] {
                                print("in dominantColors")
                                
                                if let colors = dominantColors[OperatorClient.JSONResponseKeys.Colors] as? [[String: AnyObject]] {
                                    print("in colors")
                                    
                                    for color in colors {
                                        print("in current color")
                                        
                                        if let color = color[OperatorClient.JSONResponseKeys.Color] as? [String: AnyObject] {
                                            blueComp += (color["blue"] != nil) ? color["blue"] as! Double : 0.0
                                            greenComp += (color["green"] != nil) ? color["green"] as! Double : 0.0
                                            redComp += (color["red"] != nil) ? color["red"] as! Double : 0.0
                                        }
                                    }
                                                                        
                                    (UIApplication.sharedApplication().delegate as! AppDelegate).currentScore
                                        = Int(kinematicsScore + imageScore + (redComp * 1.0)/(redComp + greenComp + blueComp) * 100)
                                    (UIApplication.sharedApplication().delegate as! AppDelegate).primaryText = self.extractPrimaryRisk(0,kScore: 0)
                                    (UIApplication.sharedApplication().delegate as! AppDelegate).secondaryText = self.extractSecondaryRisk(0,kScore: 0)
                                    (UIApplication.sharedApplication().delegate as! AppDelegate).tertiaryText = self.extractTertiaryRisk()
                                    (UIApplication.sharedApplication().delegate as! AppDelegate).summaryText = self.extractSummary(kinematicsScore, iScore: imageScore)
                                    
                                    dispatch_async(dispatch_get_main_queue()) {
                                        if let delegate = self.delegate {
                                            delegate.makeDynamicChanges?()
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
    
    func extractPrimaryRisk(iScore: Double, kScore: Double) -> String {
        if iScore > kScore {
            return "Surgical Context";
        } else {
            return "Surgeon Risk";
        }
    }
    
    func extractSecondaryRisk(iScore: Double, kScore: Double) -> String {
        if kScore >= iScore {
            return "Surgical Context";
        } else {
            return "Surgeon Risk";
        }
    }
    
    func extractTertiaryRisk() -> String { return "None" }
    
    func extractSummary(kScore: Double, iScore: Double) -> String {
        if kScore >= iScore {
            return "Our kinematic analysis reflects greater variation from what we would expect in the surgeon's gestures to be a current risk to patient safety."
        } else {
            return "Our image analysis reflects that the current surgical context is a current risk to patient safety."
        }
    }
    
    func crop(image: UIImage, rect: CGRect) -> UIImage {
       
        let imageRef = CGImageCreateWithImageInRect(image.CGImage, rect)
        
        guard imageRef != nil else {
            return UIImage()
        }
        
        let cropped = UIImage(CGImage: imageRef!)
        return cropped
    }
    
    func takeImage(maxX: CGFloat, minX: CGFloat, maxY: CGFloat, minY: CGFloat) -> UIImage {
        
        let imageRef = SessionViewController.videoSnapshot()
        
        if imageRef == nil {
            return UIImage()
        }
        
        let image = crop(imageRef!, rect: CGRect(x: minX, y: minY, width: maxX - minX, height: maxY - minY))
        (UIApplication.sharedApplication().delegate as! AppDelegate).snapshot = image
        return image
    }
    
    func takeSnapshot() {
        
        if snapshot.count > 0 {
            
            let context = UIGraphicsGetCurrentContext()
            CGContextSetLineCap(context, CGLineCap.Butt)
            CGContextSetLineWidth(context, 2)
        
            for i in 0...(snapshot.count-1) {
                CGContextBeginPath(context)
                CGContextMoveToPoint(context, snapshot[i].x, snapshot[i].y)
                CGContextAddLineToPoint(context, snapshot[(i+1)%snapshot.count].x, snapshot[(i+1)%snapshot.count].y)
                CGContextSetStrokeColorWithColor(context,
                    UIColor(red: 30/255.0, green: 215/255.0, blue: 96/255.0, alpha: 1).CGColor)
                CGContextStrokePath(context)
            }
        }
    }
}

extension SessionView {
    
    func saveImage(image: UIImage, path: String) -> Bool {
        let pngImageData = UIImagePNGRepresentation(image)
        let result = pngImageData!.writeToFile(path, atomically: true)
        return result;
    }
    
    func resizeImage(imageSize: CGSize, image: UIImage) -> NSData {
        UIGraphicsBeginImageContext(imageSize)
        image.drawInRect(CGRectMake(0, 0, imageSize.width, imageSize.height))
        let newImage = UIGraphicsGetImageFromCurrentImageContext()
        let resizedImage = UIImagePNGRepresentation(newImage)
        UIGraphicsEndImageContext()
        return resizedImage!
    }
    
    func base64EncodeImage(image: UIImage) -> String {
        var imagedata = UIImagePNGRepresentation(image)
        
        // Resize the image if it exceeds the 2MB API limit
        if (imagedata?.length > 2097152) {
            let oldSize: CGSize = image.size
            let newSize: CGSize = CGSizeMake(800, oldSize.height / oldSize.width * 800)
            imagedata = resizeImage(newSize, image: image)
        }
        
        return imagedata!.base64EncodedStringWithOptions(.EncodingEndLineWithCarriageReturn)
    }
    
    func generateKinematics() -> [Double] {
        var kinematics = [Double](count: 76, repeatedValue: 0.0)
        for i in 0...(kinematics.count-1) {
            kinematics[i] = Double(arc4random_uniform(5) + 1)
        }
        return kinematics;
    }
}

extension UIColor {
    convenience init(red: Int, green: Int, blue: Int) {
        assert(red >= 0 && red <= 255, "Invalid red component")
        assert(green >= 0 && green <= 255, "Invalid green component")
        assert(blue >= 0 && blue <= 255, "Invalid blue component")
        
        self.init(red: CGFloat(red) / 255.0, green: CGFloat(green) / 255.0, blue: CGFloat(blue) / 255.0, alpha: 1.0)
    }
    
    convenience init(netHex:Int) {
        self.init(red:(netHex >> 16) & 0xff, green:(netHex >> 8) & 0xff, blue:netHex & 0xff)
    }
}