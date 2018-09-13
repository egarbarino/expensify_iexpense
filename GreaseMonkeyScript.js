// ==UserScript==
// @name        Fill Line Item
// @namespace   tesira.com
// @include     https://ebsprod.corp.mphasis.com/OA_HTML/OA.jsp?page=/oracle/apps/ap/oie/entry/lines/webui/CashAndOtherLinesPG*
// @include     https://ebsprod.corp.mphasis.com/OA_HTML/OA.jsp?page=/oracle/apps/ap/oie/entry/lines/webui/DetailsPG*
// @version     1
// @grant       none
// ==/UserScript==

// This script is customised for Mphasis-customised version of Oracle iExpense only

// The expenses array is copy pasted since GreaseMonkey does allow reading external files
// This file is converted from Expensify CSV format using a Haskell script

var expenses = [
   { "date":"22-Jul-17", "title":"Merchant", "currency":"USD", "amount":"37.05", "category":"Meals" },
   { "date":"27-Jul-17", "title":"Joseph", "currency":"USD", "amount":"30.70", "category":"Meals" },
   { "date":"24-Jul-17", "title":"Merchant", "currency":"USD", "amount":"23.05", "category":"Meals" },
   { "date":"21-Jul-17", "title":"Food Hall", "currency":"USD", "amount":"26.80", "category":"Meals" },
   { "date":"19-Jul-17", "title":"Canada Government", "currency":"USD", "amount":"7.00", "category":"Other" },
   { "date":"19-Jul-17", "title":"Air Canada", "currency":"GBP", "amount":"130.00", "category":"Transportation" },
   { "date":"19-Jul-17", "title":"Uber", "currency":"GBP", "amount":"13.52", "category":"Transportation" },
   { "date":"19-Jul-17", "title":"Uber", "currency":"GBP", "amount":"24.65", "category":"Transportation" },
   { "date":"20-Jul-17", "title":"Bisoux Market", "currency":"USD", "amount":"3.96", "category":"Meals" },
   { "date":"20-Jul-17", "title":"Taxi", "currency":"USD", "amount":"32.40", "category":"Transportation" },
   { "date":"20-Jul-17", "title":"Hmshost Heathrow Leon T2", "currency":"GBP", "amount":"2.30", "category":"Meals" },
   { "date":"21-Jul-17", "title":"Target", "currency":"USD", "amount":"3.22", "category":"Meals" },
   { "date":"21-Jul-17", "title":"Uber", "currency":"USD", "amount":"16.86", "category":"Transportation" },
   { "date":"21-Jul-17", "title":"Uber", "currency":"USD", "amount":"15.19", "category":"Transportation" },
   { "date":"22-Jul-17", "title":"Uber", "currency":"USD", "amount":"35.88", "category":"Transportation" },
   { "date":"22-Jul-17", "title":"Uber", "currency":"USD", "amount":"15.20", "category":"Transportation" },
   { "date":"23-Jul-17", "title":"Mama", "currency":"USD", "amount":"8.12", "category":"Meals" },
   { "date":"23-Jul-17", "title":"Mint Market", "currency":"USD", "amount":"39.46", "category":"Meals" },
   { "date":"23-Jul-17", "title":"Sam Adams Brewhouse", "currency":"USD", "amount":"1.64", "category":"Meals" },
   { "date":"23-Jul-17", "title":"Hilton", "currency":"USD", "amount":"50.94", "category":"Lodging" },
   { "date":"23-Jul-17", "title":"Uber", "currency":"USD", "amount":"38.69", "category":"Transportation" },
   { "date":"23-Jul-17", "title":"Uber", "currency":"USD", "amount":"35.69", "category":"Transportation" },
   { "date":"25-Jul-17", "title":"Racetrac", "currency":"USD", "amount":"2.00", "category":"Meals" },
   { "date":"25-Jul-17", "title":"O'desi Aroma", "currency":"USD", "amount":"22.16", "category":"Meals" },
   { "date":"26-Jul-17", "title":"Fretta", "currency":"USD", "amount":"11.83", "category":"Meals" },
   { "date":"26-Jul-17", "title":"Urban Pizza Bar", "currency":"USD", "amount":"5.41", "category":"Meals" },
   { "date":"26-Jul-17", "title":"Uber", "currency":"USD", "amount":"36.38", "category":"Transportation" },
   { "date":"27-Jul-17", "title":"Starbucks", "currency":"USD", "amount":"3.11", "category":"Meals" },
   { "date":"27-Jul-17", "title":"Baskin Robbins", "currency":"USD", "amount":"5.29", "category":"Meals" },
   { "date":"27-Jul-17", "title":"Hudson News", "currency":"USD", "amount":"19.48", "category":"Meals" },
   { "date":"27-Jul-17", "title":"Uber", "currency":"USD", "amount":"29.34", "category":"Transportation" },
   { "date":"28-Jul-17", "title":"London Taxi Journey", "currency":"GBP", "amount":"38.50", "category":"Uncategorized" }
];


projectId = "71550"; // Mphasis specific
taskId    = "4.1";   // Mphasis specific
organisation = "MS UK.Support"; // Mphasis specific

if (document.title.indexOf("Details for Line") > 0) { detailsForLine(); }
if (document.title.indexOf("Allocations") > 0) { allocations(); }

// detailsForLine();

function allocations() {
  message("Allocations: Detected");  
    var allElements = document.getElementsByTagName("input");
    for (var i = 0, n = allElements.length; i < n; ++i) {
      var el = allElements[i];
      if (el.id) {  
          if (el.id.indexOf(":ProjectNumber:") > 0) {
              message (el.id);
              document.getElementById(el.id).value = projectId.toString(); 
          }      
          if (el.id.indexOf(":TaskNumber:") > 0) {
              message (el.id);
              document.getElementById(el.id).value = taskId.toString(); 
          }
      }
    }
   message("Allocations: Completed");   
}


function detailsForLine() {
  
  message("Line Detail: Detected");
  
  title = ""+document.title;
  item = title.replace( /^\D+/g, '');
  if (item > expenses.length) {

  document.title = "You've reached the end of your expenses"  

  } else {

    expense = expenses[(item-1)];

    expenseType = document.getElementById("ExpTypeChoice");
    if (expense.category == "Transportation") {
      expenseType.selectedIndex = 26;    
    } else {
      expenseType.selectedIndex = 28;
    }

    startDate = document.getElementById("StartDate");
    startDate.value = expense.date; //"26-Jun-2016";

    amount = document.getElementById("DetailReceiptAmount");
    firstAmount = amount.value;
    amount.value = expense.amount; //("15.50");

    justification = document.getElementById("Justification");
    justification.value = (item + "-" + expense.category).substring(0,10);

    currency = document.getElementById("ReceiptCurrencyChoice");
    switch (expense.currency) {
	   case "EUR": currency.selectedIndex = 24;
	               break;
       case "GBP": currency.selectedIndex = 26;
	               break;
       case "GBP": currency.selectedIndex = 26;
	               break;	
       case "INR": currency.selectedIndex = (26+8);
	               break;	
       case "USD": currency.selectedIndex = 82;
	               break;					   
    }

    // Search for Justification Button
    var allElements = document.getElementsByTagName("input");
    var allIds = [];
    var justId = "";
    for (var i = 0, n = allElements.length; i < n; ++i) {
      var el = allElements[i];
      if (el.id) { allIds.push(el.id); }
      if (el.id.startsWith("DFF_")) { justId = el.id; }
    }


    if (firstAmount == "") {
       currency.onchange();
    }

    if (justId != "") {
      document.getElementById(justId).value = expense.category + ": " + expense.title;
    }

    message("Greasemonkey Finished for: " + expense.title);

  }

}




function message(mymessage) {
  label = document.getElementById("OIE_EXPENSES_TAB_BAR");
  if (label != undefined) {
    label.innerHTML = mymessage;
  }
}

