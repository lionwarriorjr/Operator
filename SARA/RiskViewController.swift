//
//  RiskViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 9/24/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit

class RiskViewController: UITableViewController {
    
    var risks = [String]()
    var selected = 0
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.edgesForExtendedLayout = UIRectEdge.None
        self.navigationController?.navigationBar.barTintColor = UIColor(red: 50/255.0, green: 50/255.0, blue: 50/255.0, alpha: 1)
    }
    
    override func prefersStatusBarHidden() -> Bool {
        return true;
    }
    
    // MARK: - Table view data source
    
    override func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return risks.count;
    }
    
    override func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCellWithIdentifier("cell")! as UITableViewCell
        cell.detailTextLabel?.text = risks[indexPath.row]
        if selected == indexPath.row {
            cell.highlighted = true
        }
        return cell;
    }
    
    override func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        selected = indexPath.row
        (UIApplication.sharedApplication().delegate as! AppDelegate).currentRisk = (tableView.cellForRowAtIndexPath(indexPath)!.detailTextLabel?.text)!
        print((UIApplication.sharedApplication().delegate as! AppDelegate).currentRisk)
    }
}
