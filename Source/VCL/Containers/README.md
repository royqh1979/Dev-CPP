# DCContainers

Delphi containers library written for Delphi 7 as replacement for formerly used TStringList and DeCAL library.
It contains maps and sets, both with integer and string keys, where maps can store strings, integers and objects (descendants of TObject).


#### Technical note:
Each container require object manager (or better say "provider") and hash generator. Ownership of both objects is taken by container, and so they are released when it is destroyed.
Object manager provides and manages life cycle of TDCTreeKeyValue records stored in red-black tree. Currently there is only one implementation internally storing objects in list.
Hash generator calculates hashes of key values, and currently there is only one generator implementing Bob Jenkins' 32 bit hash lookup3.


#### Library uses (and depends on):
* DUnit - to compile and execute tests
* crc/hash library by Wolfgang Ehrhardt - for calculating BJL3 hashes
* RBTree by Freek van Walderveen, Jani Mátyás - for Red-Black tree implementation


#### Sample usage:

```delphi
map : TDCMapString;
dcptr : PDCTreeKeyValue;

map:=TDCMapString.Create(TDCManagerList.Create, TDCHashBJL3.Create);
map.Add('key1', 'example value');
map.Add('key2', 12345);
//...
dcptr:=map.Find('key1');
if dcptr <> nil then
	ShowMessage(dcptr^.Value.AsString);
```

More detailed usage can be found in _Tests_.
