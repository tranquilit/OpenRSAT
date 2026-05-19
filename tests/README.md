# OpenRSAT test suite

OpenRSAT test suite objective's is to define the behavior of a feature. 
Each feature in the project has an objective: create a group object, move a user object, delegate rights... and testing each feature ensure is does what it says.
To do so, tests must be implemented alongside a feature developement, and a new bug requires a new test to define the expected behavior before fixing the feature.

## Tests structure

OpenRSAT test suite contains different kind of tests:
- **`unit`**: Ensures the reliability of logical functions. Does not require external dependency.
- **`integration`**: Ensures the reliability of network calls. Requires an Active Directory to run.
- **`gui`**: Ensures the reliability of the user interface's behavior. Requires a Desktop Environement to run.

## Tests result

Test result are not yet provided.
A badge with the test result, which redirect to the latest test result should be provided.

## Test automation

Test will be automatically run each day. A report should be available online after each run.