//
//  Constants.swift
//  SARA
//
//  Created by Srihari Mohan on 9/24/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import Foundation

extension OperatorClient {
    
    struct Constants {
        
        // MARK: API Key
        static let GoogleVisionApiKey : String = "AIzaSyB4EZ3kM1Ore9AR18lUoo7ybf7GwzwJ-kM"
        static let GoogleCalendarApiKey: String = "828953204650-b0scbg4i2rbkehhdrsbm809qf08l6nu3.apps.googleusercontent.com"
        
        // MARK: URLs
        static let WatsonApiKey = "b0fe95d926df54b9061b5bb427f33bc0e7e1a023"
        static let WatsonClassifierID = "Risk_1181143906"
        static let WatsonVersion = "2016/09/24"
        static let WatsonOwners = "me"
        static let ApiScheme = "https"
        static let ApiHost = "vision.googleapis.com"
    }
    
    // MARK: Methods
    struct Methods {
        // MARK: Google Vision API
        static let Analyze = "/v1/images:annotate"
        static let WatsonVision = "/v3/classify"
    }
    
    struct ParameterKeys {
        static let Requests = "requests"
        static let Image = "image"
        static let Content = "content"
        static let Features = "features"
        static let Type = "type"
        static let MaxResults = "maxResults"
    }
    
    // MARK: JSON Response Keys
    struct JSONResponseKeys {
        static let Responses = "responses"
        static let ImagePropertiesAnnotation = "imagePropertiesAnnotation"
        static let DominantColors = "dominantColors"
        static let Colors = "colors"
        static let Color = "color"
    }
}