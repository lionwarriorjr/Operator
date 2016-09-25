//
//  Move.swift
//  SARA
//
//  Created by Srihari Mohan on 9/23/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit

struct Move {
    var start: CGPoint
    var end: CGPoint
    
    init(start _start: CGPoint, end _end: CGPoint) {
        self.start = _start
        self.end = _end
    }
}
