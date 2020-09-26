// Uncomment the following directive to create a console application
// or leave commented to create a GUI application... 
// {$APPTYPE CONSOLE}

program Tests;

uses
//  FastMM4,
  TestFramework {$IFDEF LINUX},
  QForms,
  QGUITestRunner {$ELSE},
  Forms,
  GUITestRunner {$ENDIF},
  TextTestRunner,
  rbtree in '..\RBTree\RBTree.pas',
  RBTreeTraverse in '..\RBTree\RBTreeTraverse.pas',
  RBTreeTypes in '..\RBTree\RBTreeTypes.pas',
  TestsRBTree in 'TestsRBTree.pas',
  TestsRBTreeTraverse in 'TestsRBTreeTraverse.pas',
  U_RBTreeTestDataObjects in 'U_RBTreeTestDataObjects.pas',
  U_RBTreeTestTypes in 'U_RBTreeTestTypes.pas',
  U_DCTree in '..\Containers\U_DCTree.pas',
  U_DCTreeKeyValue in '..\Containers\U_DCTreeKeyValue.pas',
  U_DCHashBase in '..\Containers\U_DCHashBase.pas',
  U_DCHashJenkins in '..\Containers\U_DCHashJenkins.pas',
  TestsDCHashJenkins in 'TestsDCHashJenkins.pas',
  U_DCManagerList in '..\Containers\U_DCManagerList.pas',
  TestsDCManagerList in 'TestsDCManagerList.pas',
  U_DCManagerBase in '..\Containers\U_DCManagerBase.pas',
  U_DCValue in '..\Containers\U_DCValue.pas',
  TestsDCValue in 'TestsDCValue.pas',
  TestsDCTreeKeyValue in 'TestsDCTreeKeyValue.pas',
  U_DCRBTContainer in '..\Containers\U_DCRBTContainer.pas',
  U_DCMapInt in '..\Containers\U_DCMapInt.pas',
  TestsDCMapInt in 'TestsDCMapInt.pas',
  U_DCMapString in '..\Containers\U_DCMapString.pas',
  TestsDCMapString in 'TestsDCMapString.pas',
  U_DCExceptions in '..\Containers\U_DCExceptions.pas',
  U_DCValueInteger in '..\Containers\U_DCValueInteger.pas',
  U_DCValueString in '..\Containers\U_DCValueString.pas',
  U_DCValueObject in '..\Containers\U_DCValueObject.pas',
  TestsDCMapBase in 'TestsDCMapBase.pas',
  U_DCSetString in '..\Containers\U_DCSetString.pas',
  U_DCSetInt in '..\Containers\U_DCSetInt.pas',
  TestsDCSetString in 'TestsDCSetString.pas',
  TestsDCSetInt in 'TestsDCSetInt.pas',
  TestsDCRBTContainer in 'TestsDCRBTContainer.pas',
  BJL3 in '..\BJL\bjl3.pas',
  BTypes in '..\BJL\btypes.pas',
  U_DCHashBJL3 in '..\Containers\U_DCHashBJL3.pas',
  TestsDCHashBase in 'TestsDCHashBase.pas',
  TestsDCHashBJL3 in 'TestsDCHashBJL3.pas',
  U_DCTreeVisualization in '..\Containers\U_DCTreeVisualization.pas';

{$R *.RES}

begin
  Application.Initialize;

{$IFDEF LINUX}
  QGUITestRunner.RunRegisteredTests;
{$ELSE}
  if System.IsConsole then
    TextTestRunner.RunRegisteredTests
  else
    GUITestRunner.RunRegisteredTests;
{$ENDIF}

end.

 