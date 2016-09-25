//
//  ToolsViewController.swift
//  SARA
//
//  Created by Srihari Mohan on 9/24/16.
//  Copyright © 2016 Srihari Mohan. All rights reserved.
//

import UIKit

class ToolsViewController: UITableViewController {
    
    var sects = [String]()
    var items = [[String]]()
    var toolImages = [[UIImage]]()
    
    override func viewDidLoad() {
        super.viewDidLoad()
        self.edgesForExtendedLayout = UIRectEdge.None
        self.navigationController?.navigationBar.barTintColor = UIColor(red: 50/255.0, green: 50/255.0, blue: 50/255.0, alpha: 1)
        sects = ["Standard Surgical Tools", "Robotically-Assisted Surgical Tools"]
        items = [["Grasping Forceps", "Bipolar Forceps", "Retractor",
            "Scissors", "Titanium Clip Applicator", "Polymer Clip Applicator"],
        ["Forceps", "Needle Drivers",
            "Retractors"]]
    }
    
    override func prefersStatusBarHidden() -> Bool {
        return true;
    }
    
    // MARK: - Table view data source
    
    override func tableView(tableView: UITableView, titleForHeaderInSection section: Int) -> String? {
        return sects[section].lowercaseString;
    }
    
    override func numberOfSectionsInTableView(tableView: UITableView) -> Int {
        return self.sects.count;
    }
    
    override func tableView(tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
        return items[section].count;
    }
    
    override func tableView(tableView: UITableView, cellForRowAtIndexPath indexPath: NSIndexPath) -> UITableViewCell {
        let cell = tableView.dequeueReusableCellWithIdentifier("cell", forIndexPath: indexPath) as! ToolCell
        if indexPath.section < toolImages.count && toolImages[indexPath.section].count > indexPath.row {
            cell.toolImage?.image = toolImages[indexPath.section][indexPath.row]
        }
        cell.toolLabel?.text = items[indexPath.section][indexPath.row]
        if indexPath.section != 0 || indexPath.row != 0 {
            cell.toolLabel?.textColor = randomColor(luminosity: .Bright)
        } else {
            cell.toolLabel?.textColor = UIColor(netHex: 0x1ca0de)
        }
        return cell;
    }
    
    override func tableView(tableView: UITableView, willDisplayHeaderView view: UIView, forSection section: Int) {
        let headerView = view as! UITableViewHeaderFooterView
        headerView.textLabel?.text = sects[section]
    }
    
    override func tableView(tableView: UITableView, didSelectRowAtIndexPath indexPath: NSIndexPath) {
        (UIApplication.sharedApplication().delegate as! AppDelegate).currentTool = ((tableView.cellForRowAtIndexPath(indexPath) as! ToolCell).toolLabel?.text)!
        (UIApplication.sharedApplication().delegate as! AppDelegate).currentColor = ((tableView.cellForRowAtIndexPath(indexPath) as! ToolCell).toolLabel?.textColor)!
    }
}
