//
//  OperatorClient.swift
//  SARA
//
//  Created by Srihari Mohan on 9/23/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit

class OperatorClient {
    
    static func taskForPOSTVisionMethod(imageData: NSString, completionHandlerForPOST: (result: AnyObject!, error: NSError?) -> Void) -> NSURLSessionDataTask {
        print("Calling Vision API")
        let request = OperatorClient.setupVisionRequest(imageData)
        return OperatorClient.setupTask(request, completionHandlerForPOST: completionHandlerForPOST);
    }
    
    static func taskForWatsonVisionMethod(imageURL: String, completionHandlerForPOST: (result: AnyObject!, error: NSError?) -> Void) -> NSURLSessionDataTask {
        print("Calling Vision API")
        let request = OperatorClient.setupWatson(imageURL)
        return OperatorClient.setupTask(request, completionHandlerForPOST: completionHandlerForPOST);
    }
    
    static func taskForR(x: [Double], completionHandlerForPOST: (result: AnyObject!, error: NSError?) -> Void) -> NSURLSessionDataTask {
        print("Executing R script")
        let request = OperatorClient.setupRRequest(x)
        return OperatorClient.setupTask(request, completionHandlerForPOST: completionHandlerForPOST);
    }
    
    static func setupRRequest(x: [Double]) -> NSMutableURLRequest {
        print("Setting up R request")
        let request = NSMutableURLRequest(
            URL: NSURL(string: "https://trial.dominodatalab.com/v1/suemo/quick-start/endpoint")!)
        request.HTTPMethod = "POST"
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")
        request.addValue("nse7lunCzYtjkGiHq44eUw1iOoAq9ny091qn3bilvGHEm6lnH7d3f51SxuNUYY5M", forHTTPHeaderField: "X-Domino-Api-Key")
        let jsonRequest: [String: AnyObject] = [
            "parameters": [
                x
            ]
        ]
        
        print("Serialize the JSON")
        
        do {
            request.HTTPBody = try NSJSONSerialization.dataWithJSONObject(jsonRequest, options: [])
        }  catch {
            print("Could not populate JSON request in Google Vision API call")
        }
        
        return request;
    }
    
    static func setupWatson(imageURL: String) -> NSMutableURLRequest {
        print("Setting up Watson Call")
        let request = NSMutableURLRequest(
            URL: NSURL(string: "https://gateway-a.watsonplatform.net/visual-recognition/api/v3/classify?api_key=b0fe95d926df54b9061b5bb427f33bc0e7e1a023&version=2016-05-20")!)
        request.HTTPMethod = "POST"
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")
        request.addValue(imageURL, forHTTPHeaderField: "images_file")
        let parameters: [String: AnyObject] = [
            "images_file": imageURL,
            "classifier_ids": [
                OperatorClient.Constants.WatsonClassifierID
            ],
            "owners": [
                OperatorClient.Constants.WatsonOwners
            ]
        ]
        do {
            request.HTTPBody = try NSJSONSerialization.dataWithJSONObject(parameters, options: [])
        } catch {
            print("Could not populate JSON request in Watson API call")
        }
        return request;
    }
    
    static func setupVisionRequest(imageData: NSString) -> NSMutableURLRequest {

        print("Setting up Vision API Call")
        let request = NSMutableURLRequest(
            URL: NSURL(string: "https://vision.googleapis.com/v1/images:annotate?key=AIzaSyB4EZ3kM1Ore9AR18lUoo7ybf7GwzwJ-kM")!)
        request.HTTPMethod = "POST"
        request.addValue("application/json", forHTTPHeaderField: "Content-Type")
        let jsonRequest: [String: AnyObject] = [
            "requests": [
                "image": [
                    "content": imageData
                ],
                "features": [
                    [
                        "type": "IMAGE_PROPERTIES",
                        "maxResults": 1
                    ]
                ]
            ]
        ]
    
        print("Serialize the JSON")
        
        do {
            request.HTTPBody = try NSJSONSerialization.dataWithJSONObject(jsonRequest, options: [])
        }  catch {
            print("Could not populate JSON request in Google Vision API call")
        }
        
        return request;
    }
    
    static func setupTask(request: NSMutableURLRequest, completionHandlerForPOST: (result: AnyObject!, error: NSError?) -> Void) -> NSURLSessionDataTask {
        
        let task = NSURLSession.sharedSession().dataTaskWithRequest(request) { (data, response, error) in
            
            func sendError(error: String) {
                NSLog(error)
                let userInfo = [NSLocalizedDescriptionKey : error]
                completionHandlerForPOST(result: nil, error: NSError(domain: "taskForPOSTMethod", code: 1, userInfo: userInfo))
            }
            
            if error == nil {
                
                guard let statusCode = (response as? NSHTTPURLResponse)?.statusCode where
                    statusCode >= 200 && statusCode <= 299 else {
                        sendError("Status code not of form 2xx")
                        return
                }
                
                guard let data = data else {
                    sendError("No data was returned by the request")
                    return
                }
                
                NSLog("Status Code OK -> Data Parsed")
                
                self.convertDataWithCompletionHandler(data, completionHandlerForConvertData: completionHandlerForPOST)
                
            } else {
                sendError("There was an error with your request \(error)")
                return
            }
        }
        
        task.resume()
        return task;
    }
    
    static func convertDataWithCompletionHandler(data: NSData, completionHandlerForConvertData: (result: AnyObject!, error: NSError?) -> Void) {
        
        var parsedResult: AnyObject!
        do {
            parsedResult = try NSJSONSerialization.JSONObjectWithData(data, options: .AllowFragments) as! NSDictionary
        } catch {
            let userInfo = [NSLocalizedDescriptionKey : "Could not parse the data as JSON: '\(data)'"]
            completionHandlerForConvertData(result: nil, error: NSError(domain: "convertDataWithCompletionHandler", code: 1, userInfo: userInfo))
        }
        
        print("Handled API Call -> Hand Over to Client")
        completionHandlerForConvertData(result: parsedResult, error: nil)
    }
    
    class func sharedInstance() -> OperatorClient {
        struct Singleton {
            static var sharedInstance = OperatorClient()
        }
        return Singleton.sharedInstance
    }
}
