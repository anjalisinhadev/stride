import Float "mo:base/Float";
import Int "mo:base/Int";
import Array "mo:base/Array";
import Text "mo:base/Text";
import Nat "mo:base/Nat";
import Char "mo:base/Char";
import Nat32 "mo:base/Nat32"; // Added this critical import

actor StrideTracker {
  let FEET_PER_KM : Float = 3280.84;

  // Persistent state
  stable var stepGoal : Nat = 10000;
  stable var totalSteps : Float = 0.0;
  stable var walkHistory : [Walk] = [];

  public type Walk = {
    km : Float;
    stride : Float;
    steps : Nat;
  };

  // Set step goal
  public func setGoal(inputGoal : Text) : async Text {
    if (inputGoal == "") {
      stepGoal := 10000;
      return "Default goal set to 10000 steps";
    };

    switch (parseNat(inputGoal)) {
      case (?num) {
        stepGoal := num;
        return "Goal set to " # Nat.toText(num) # " steps";
      };
      case null {
        return "Please enter a valid number for your step goal";
      };
    };
  };

  // Custom safe nat parser
  func parseNat(t : Text) : ?Nat {
    let chars = Text.toIter(t);
    var result : Nat = 0;
    var hasDigit = false;

    for (c in chars) {
      if (c >= '0' and c <= '9') {
        hasDigit := true;
        // Convert character to digit value
        let digitValue = Char.toNat32(c) - 48;
        result := result * 10 + Nat32.toNat(digitValue);
      } else {
        return null;
      }
    };
    if (hasDigit) ?result else null;
  };

  // Add a walk
  public func addWalk(kmText : Text, strideText : Text) : async Text {
    if (kmText == "" or strideText == "") {
      return "Please enter values for both fields";
    };

    let km = switch (parseFloat(kmText)) {
      case (?num) num;
      case null { return "Invalid kilometers value"; };
    };

    let stride = switch (parseFloat(strideText)) {
      case (?num) num;
      case null { return "Invalid stride length"; };
    };

    if (km <= 0.0 or stride <= 0.0) {
      return "Values must be positive numbers";
    };

    let steps = (km * FEET_PER_KM) / stride;
    let stepsInt = Int.abs(Float.toInt(steps));

    walkHistory := Array.append(walkHistory, [{
      km = km;
      stride = stride;
      steps = stepsInt;
    }]);

    totalSteps := totalSteps + steps;

    return "Added walk: " # Float.toText(km) # " km = " # Int.toText(stepsInt) # " steps";
  };

  // Working parseFloat
  func parseFloat(t : Text) : ?Float {
    let chars = Text.toIter(t);
    var result = 0.0;
    var decimalPlace = 0;
    var isDecimal = false;
    var sign : Float = 1.0;
    var hasDigit = false;

    for (c in chars) {
      if (c == '-') {
        sign := -1.0;
      } else if (c == '.') {
        if (isDecimal) return null;
        isDecimal := true;
      } else if (c >= '0' and c <= '9') {
        hasDigit := true;
        // Convert character to digit value
        let digitValue = Char.toNat32(c) - 48;
        let digit = Float.fromInt(Nat32.toNat(digitValue));
        
        if (isDecimal) {
          decimalPlace += 1;
          result := result + (digit / (10.0 ** Float.fromInt(decimalPlace)));
        } else {
          result := result * 10.0 + digit;
        }
      } else {
        return null;
      }
    };
    
    if (not hasDigit) return null;
    ?(result * sign);
  };

  // Get all walks
  public query func getWalks() : async [Walk] {
    walkHistory
  };

  // Get summary
  public query func getSummary() : async Text {
    var message = "You walked " # Int.toText(walkHistory.size()) # " times.\n";
    message := message # "Total steps: " # Float.toText(totalSteps) # "\n";

    if (totalSteps >= Float.fromInt(stepGoal)) {
      message := message # "Goal reached!";
    } else {
      message := message # "Keep going!";
    };

    message
  };
}