type Name = String
type Content = String
data FSItem = File Name Content | Folder Name [FSItem] deriving (Show)
data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

(-:) :: a -> (a -> b) -> b
x -: f = f x

goIn :: Name -> FSZipper -> FSZipper
goIn name (Folder n items, bs) =
  let (l, item:r) = break (isName name) items
  in  (item, FSCrumb n l r:bs)

isName :: String -> FSItem -> Bool
isName name (File fileName _) = name == fileName
isName name (Folder folderName _) = name == folderName

getFileContent :: FSZipper -> String
getFileContent (File _ content, _) = content

goOut :: FSZipper -> FSZipper
goOut (item, FSCrumb name l r:bs) = (Folder name (l ++ [item] ++ r), bs)

addItem :: FSItem -> FSZipper -> FSZipper
addItem newItem (Folder name items, bs) = (Folder name (newItem:items), bs)

renameItem :: Name -> FSZipper -> FSZipper
renameItem newName (File name content, bs) = (File newName content, bs)
renameItem newName (Folder name items, bs) = (Folder newName items, bs)

removeItem :: FSZipper -> FSZipper
removeItem (_, FSCrumb name l r:bs) = (Folder name (l ++ r), bs)

-- Safe API --

goInM :: Name -> FSZipper -> Maybe FSZipper
goInM name z@(Folder n items, bs) =
  if any (isName name) items then Just (goIn name z) else Nothing
goInM name (File _ _, _) = Nothing

getFileContentM :: FSZipper -> Maybe String
getFileContentM (File _ content, _) = Just content
getFileContentM (Folder _ _, _) = Nothing

goOutM :: FSZipper -> Maybe FSZipper
goOutM (item, []) = Nothing 
goOutM z = Just $ goOut z

addItemM :: FSItem -> FSZipper -> Maybe FSZipper
addItemM newItem (File _ _, _) = Nothing
addItemM newItem z = Just $ addItem newItem z

renameItemM :: Name -> FSZipper -> Maybe FSZipper
renameItemM n z = Just $ renameItem n z

removeItemM :: FSZipper -> Maybe FSZipper
removeItemM (_, []) = Nothing
removeItemM z = Just $ removeItem z

------------

fs =
  Folder "canvas"
    [ Folder "fdraw"
      [ Folder "public"
        [ File "styles.css" "dummy6"
        , File "app.js" "dummy7"
        , Folder "fdraw"
          [ File "fdraw.js" "dummy8"
          , File "fdraw.module.js" "dummy9"
          ]
        ]
      ]
    , Folder "fdraw-ng2"
      [ Folder "dist"
        [ File "index.html" "dummy1"
        , Folder "lib"
          [ File "rx.js" "dummy2"
          , File "system.src.js" "dummy3"
          ]
        ]
      , Folder "src"
        [ Folder "app"
          [ File "app.component.ts" "dummy4"
          , File "bootstrap.ts" "dummy5"
          ]
        , File "index.html" "dummy 10"
        ]
      ]
    ]

z1 = (fs, []) -: goIn "fdraw-ng2" -: goIn "dist" -: goIn "lib" -: goIn "rx.js"
t1 = z1 -: getFileContent
z2 = z1 -: goOut -: goOut -: goOut -: goIn "src" -: goIn "app" -: goIn "bootstrap.ts"
t2 = z2 -: getFileContent
z3 = (fs, []) -: goIn "fdraw" -: addItem (File "newFile" "dummy11")
z4 = z3 -: renameItem "not a new any more"
z5 = (fs, []) -: goIn "fdraw-ng2" -: goIn "dist" -: removeItem -: goOut

-- Safe API --

m11 = return (fs, []) >>= goInM "fdraw-ng2" >>= goInM "dist" >>= goInM "lib" >>= goInM "rx.js"
m12 = return (fs, []) >>= goInM "fdraw-ng2" >>= goInM "dist" >>= goInM "LIB" >>= goInM "rx.js"
