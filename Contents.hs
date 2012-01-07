module Contents where

import Data.Tree
import Forest (leaf)

contents :: Forest (String,String)
contents =
  [leaf ("Introduction",txt_Intro)

  ,leaf ("0 Specify the Number of Players",txt_0)

  ,Node ("1 Import Images",txt_1)
     [leaf ("1.1 Specify Image Size"   ,txt_1_1)
     ,leaf ("1.2 Select and Add Images",txt_1_2)
     ,leaf ("1.3 Edit Image Selection" ,txt_1_3)]

  ,Node ("2 Initial Board",txt_2)
     [leaf ("2.1 Specify the Board Size"     ,txt_2_1)
     ,leaf ("2.2 Construct an Empty Board"   ,txt_2_2)
     ,leaf ("2.3 Construct the Initial Setup",txt_2_3)]

  ,Node ("3 Game Events",txt_3)
     [leaf ("3.1 Add and Edit Pairs"        ,txt_3_1)
     ,leaf ("3.2 Add and Edit Choice Nodes" ,txt_3_2)

     ,Node ("3.3 Conditions",txt_3_3)
        [leaf ("3.3.1 Mouseclicks\
                 \and Cursor Hovering"  ,txt_3_3_1)
        ,leaf ("3.3.2 Key Combinations" ,txt_3_3_2)
        ,leaf ("3.3.3 Configurations"   ,txt_3_3_3)]

     ,Node ("3.4 Actions",txt_3_4)
        [leaf ("3.4.2 Game Piece Tranformations\
                 \and Game Piece Appearances"  ,txt_3_4_1)
        ,leaf ("3.4.4 A Game Piece Disappears" ,txt_3_4_2)]]]

txt_Intro =
  "\n\
  \Introduction\n\
  \--------------\n\
  \\n\
     \\tBoard Game Creater (BGC) is a program that allows\
  \ users to enter information about a turn-based board\
  \ game. That board game is implemented by putting unit\
  \ images on a rectangular lattice, and changes that happen\
  \ to the board, like the movement of a game pieces, is\
  \ carried out manipulating the unit images. BGC provides\
  \ the interface for users to specify the initial state of\
  \ the board and the game behavior.\n\
  \\n\
     \\tThe BGC program has three main parts to the process\
  \ of data collection, and they correspond to the three tabs\
  \ seen in the interface. In \"Import Images\", users add\
  \ images that are used in the game. In \"Initial Board\",\
  \ users fill in the board with images, and finally, in\
  \ \"Game Events\", users specify the game behavior.\n\
  \\n\
     \\tIn BGC, there are many buttons that only have images\
  \ on them. The following lists the meanings of the more\
  \ common ones.\n\
  \\n\
                 \\t\t\tButton Legend:\n\
                 \\t\t\t-----------------\n\
                 \\t\tImage:\t\tMeaning:\n\
                 \\t\tGreen Circle\tOk\n\
                 \\t\tRed Circle\t\tCancel\n\
                 \\t\t\"+\"\t\t\tAdd\n\
                 \\t\t\"-\"\t\t\tRemove\n\
                 \\t\tPen on Paper\tEdit\n\
                 \\t\tBroom\t\tClear\n\
  \\n\
     \\tAdd and Remove usually have Return (Enter) and Delete\
  \ as their accelerator (shortcut) keys.\n"

txt_0 =
  "\n\
  \Specify Number of Players\n\
  \------------------------------\n\
  \\n\
     \\tThe first thing users need to specify in BGC is the\
  \ number of players in the game. The defualt value is 2,\
  \ but if users wish the edit it, they may do so by clicking\
  \ the Edit button beside it. A Dialog box will pop up,\
  \ requesting the number of players. Because some specifications\
  \ will be unique to each player later on, once a player is\
  \ removed (e.g. Player 4 is removed when the player number\
  \ shrnks from 5 to 2) he may be left in the specifications\
  \ when he should be removed. Hence, if the player number is\
  \ decreased, a warning message appears saying that if the\
  \ player number is decreased, all data in the \"Game Events\"\
  \ tab will be erased. This does happen if the user proceeds\
  \ with decreasing the number of players, but not otherwise.\
  \ Users are advised not to change this number too often,\
  \ thinking carefully before using BGC and putting the correct\
  \ player number right at the start, removing the need to\
  \ change it again.\n"

txt_1 =
  "\n\
  \Import Images\n\
  \-----------------\n\
  \\n\
     \\tTwo of the things in BGC that need images are the\
  \ empty board and the game pieces. The empty board needs\
  \ to be tiled with images (which will not change when the\
  \ game is played), and each game piece is a unit image itself.\
  \ However, the body of each game piece is opaque, but not\
  \ the background. This makes visible the parts of the empty\
  \ board not covered by the body of the piece. The transparency\
  \ of the game piece images is left to the users to make.\n"

txt_1_1 =
  "\n\
  \Import Images\n\
  \  >> Specify Image Size\n\
  \----------------------------\n\
  \\n\
     \\tBefore any images are added, the image size must be\
  \ specified. By clicking the \"Edit Image Size\" button,\
  \ users can specify it using the dialog box that appears.\
  \ Every image that is imported will be scaled to that size,\
  \ without preserving the aspect ratio. This can potentially\
  \ distort the images, so it is advisable to prepare images\
  \ that already have the same size, and enter that size as\
  \ the image size. Whenever the image size is changed again,\
  \ every single imported image in BGC will be scaled to the\
  \ new size again.\n"

txt_1_2 =
  "\n\
  \Import Images\n\
  \  >> Select and Add Images\n\
  \---------------------------------\n\
  \\n\
     \\tThe file chooser provided here operates just like\
  \ the normal ones used to open files, except that instead\
  \ of a button to \"go up one level\", this file chooser\
  \ has a series of buttons at the top describing the path\
  \ of the current directory. Clicking on any of those buttons\
  \ would allow one to navigate to the directory on the button.\
  \ Once the Add button below is clicked, the selected image\
  \ files will be added to the pane on the right. The file\
  \ chooser stays there all the time, so users can feel free\
  \ to navigate around and add images from many different\
  \ directories, but that is discouraged; it is recommended\
  \ that users group the images they want in a single folder\
  \ for ease of access and maintenance.\n\
  \\n\
     \\tThe type of files that are visible in the file chooser,\
  \ as stated in the \"menu\" above the Add button, are \"Supported\
  \ Image Files\". The allowed extensions for the image files\
  \ are as follows: png, bmp, wbmp, gif, ico, ani, jpeg, pnm,\
  \ ras, tiff, xpm, xbm, tga.\n"

txt_1_3 =
  "\n\
  \Import Images\n\
  \  >> Edit Image Selection\n\
  \------------------------------\n\
  \\n\
     \\tAdding images has been covered in the previous\
  \ section, so now the only operations are removing\
  \ images and clearing the whole selection. Just like\
  \ decreasing the number of players, since images become\
  \ embedded in the \"Initial Board\" and \"Game Events'\"\
  \ tabs, removing some of them must also remove them from\
  \ those tabs. When the Remove button is clicked, a warning\
  \ message will appear, and if Ok is clicked, the selected\
  \ images to the right are removed the copies of those\
  \ images in the \"Initial Board\" tab are changed into\
  \ empty placeholders, and the \"Game Events\" tab is\
  \ cleared. The same goes for the Clear button, which\
  \ removes all the imported images. Hence, users are\
  \ advised to finalise the image selection as soon as\
  \ possible to avoid possible complications.\n"

txt_2 =
  "\n\
  \Initial Board\n\
  \--------------\n\
  \\n\
     \\tTwo tasks need to be completed in this tab. An\
  \ empty board needs to be contructed by tiling it with\
  \ images, and the initial setup of game pieces also\
  \ needs to be created.\n"

txt_2_1 =
  "\n\
  \Initial Board\n\
  \  >> Specify Board Size\n\
  \---------------------------\n\
  \\n\
     \\tBy clicking the \"Edit Board Size\" button,\
  \ users can edit the board size, i.e. the number of\
  \ rows and columns. The moment both the board size\
  \ and image size have been specified, a grid of blue\
  \ rectangles (empty placeholders) with their size\
  \ being the image size appears on both sub-tabs\
  \ \"Empty Board\" and \"Initial Setup\". That grid\
  \ will have the specified number of rows and columns.\
  \ The grids on both sub-tabs will eventually contain\
  \ images, and when the board size is changed again,\
  \ the positions of the old grid that were not removed\
  \ retain the images stored there and the added positions\
  \ will be occupied by empty placeholders. The images\
  \ on removed positions cannot be recovered.\n"

txt_2_2 =
  "\n\
  \Initial Board\n\
  \  >> Construct an Empty Board\n\
  \------------------------------------\n\
  \\n\
     \\tEach position on the grid in the \"Empty Board\"\
  \ sub-tab can hold one image. Upon the activation\
  \ (selecting and pressing Enter or double-clicking)\
  \ of an image in the menu on the right, the activated\
  \ image replaces the images (or placeholders) at the\
  \ selected positions in the grid. Users are to replace\
  \ all of the placeholders with imported images before\
  \ proceeding.\n"

txt_2_3 =
  "\n\
  \Initial Board\n\
  \  >> Construct an Initial Setup\n\
  \-----------------------------------\n\
  \\n\
     \\tThe starting appearance of the grid in the \"Initial\
  \ Setup\" sub-tab is identical to the current appearance\
  \ of the grid in the \"Empty Board\" sub-tab. Now, each\
  \ position on the board can hold any number of game pieces\
  \ in a certain stacking order. When an image in the menu\
  \ on the right is activated, it is placed on top of all\
  \ the selected positions in the grid. The order of a stack\
  \ of game pieces on a certain position is determined by\
  \ the order of arrival; the latest ones are on top, and\
  \ the oldest ones are at the bottom. Pressing Delete removes\
  \ the topmost game piece of each selected game piece stack.\
  \ Users are to construct the initial setup of game pieces\
  \ before proceeding.\n"

txt_3 =
  "\n\
  \Game Events\n\
  \---------------\n\
  \\n\
     \\tHere is the section that requires users to specify\
  \ game behavior. Game behavior is modelled here as a set\
  \ of conditions and actions; considering a simple case,\
  \ conditions and actions exist in pairs, and when a condition\
  \ is fulfilled the action is carried out. For example,\
  \ a user may want a game piece to appear (action) at a\
  \ posiiton that was clicked (condition). More flexibility\
  \ is allowed for in BGC, for the fulfillment of conditions\
  \ is monitored by a conditions tree; more on this later.\n\
  \\n\
     \\tBeside the Add button in this tab is a menu with 4\
  \ options; adding anything in this tab is done by selecting\
  \ the correct option and clicking the Add button.\n"

txt_3_1 =
  "\n\
  \Game Events\n\
  \  >> Add and Edit Pairs\n\
  \----------------------------\n\
  \\n\
     \\tWhen adding a pair of conditions and actions, a dialog\
  \ box asks users which player's turn the pair is active for.\
  \ Only during that turn will the pair be considered. After\
  \ selecting a player and clicking Ok, A new pair is added,\
  \ and it says \"Player ..\" and branches into two nodes, the\
  \ condition node and the action node. The player number can\
  \ be edited at any time by selecting that \"Player ..\" node\
  \ and clicking the Edit button. This structure comes in one\
  \ piece and can only be removed in one shot by selecting the\
  \ Player node and clicking the Remove button, but anything\
  \ extra added to it can be removed separately and in the same\
  \ way.\n"

txt_3_2 =
  "\n\
  \Game Events\n\
  \  >> Add and Edit Choice Nodes\n\
  \-------------------------------------\n\
  \\n\
     \\tThe condition node branches to another node labelled\
  \ \"All\", which is a choice node. A choice node is a node\
  \ which can have a value of \"All\" or a positive integer\
  \ not exceeding the number of branches it has. The value of\
  \ a choice node can be edited (within its allowed range) at\
  \ any time by selecting it and clicking the Edit button. To\
  \ add a new choice node branching out from an existing choice\
  \ node, select the \"parent\" and add a choice node.\n"

txt_3_3 =
  "\n\
  \Game Events\n\
  \  >> Conditions\n\
  \------------------\n\
  \\n\
     \\tConditions can be added as the \"child\" of any\
  \ choice node. Eventually, the complex of choice nodes\
  \ (which always branch out) and conditions (which terminate\
  \ the branches) forms a conditions tree, which, when it\
  \ is active, is \"evaluated\" as thus: every condition\
  \ becomes either fulfileld or unfulfilled, and each choice\
  \ node's value states how many of its children must be\
  \ fulfilled for itself to be fulfilled. The evaluation\
  \ process continues recursively until the entire tree\
  \ is evaluated to either \"fulfilled\" or \"not fulfilled\",\
  \ the former case leading to the execution of the action.\
  \ This evaluation method gives more flexibility to the user.\n\
  \\n\
     \\tUpon clicking the Add button to add a condition,\
  \ a dialog box appears. It has a menu at the top, containing\
  \ the different kinds of condiitons that can be added.\
  \ The bottom half of the dialog changes with the option\
  \ in the menu; it usually contains a button that leads\
  \ to an editor for that specific condition. Even if a\
  \ user switches to another condition using the menu and\
  \ comes back, the infomation is not lost. When pressing\
  \ Ok, BGC will treat the current option on the menu as\
  \ the type of condition that is being edited, and will\
  \ extract information from the editor and store it. If\
  \ nothing has been specified for that condition, an error\
  \ message will appear and exiting the dialog is blocked.\
  \ The next few sections talk about the different kinds\
  \ of condiitons that can be added, as well as how to work\
  \ with all the dialogs and editors associated with them.\n"

txt_3_3_1 =
  "\n\
  \Game Events\n\
  \  >> Conditions\n\
  \    >> Mouse-clicks and Cursor Hovering\n\
  \----------------------------------------------\n\
  \\n\
     \\tThese two conditions are almost exactly the same,\
  \ as they require the same specifications. Both need\
  \ board positions or game pieces to click on, so when\
  \ the active item in the menu is a mouse-click or cursor\
  \ hovering condition, there will be two buttons, one\
  \ starting a board positions editor and the other\
  \ starting a game pieces editor. The difference is\
  \ that mouse-click conditions have a menu for choosing\
  \ between a left-click and right-click. If any one of\
  \ the board positions or game pieces specified here\
  \ was clicked (or hovered over), this condition will\
  \ be fulfilled.\n\
  \\n\
  \Board Positions Editor\n\
  \-------------------------\n\
  \\n\
     \\tThis editor displays the empty board. To add\
  \ board positions, select any number of positions\
  \ and click the Add button. To remove certain\
  \ positions, select them and click the Remove button.\
  \ To confirm a non-empty selection, click Ok.\n\
  \\n\
  \Game Pieces Editor\n\
  \----------------------\n\
  \\n\
     \\tThis editor displays the list of imported images\
  \ paired up with checkboxes. To add certain game pieces\
  \ (even the board tiles are treated as game pieces here),\
  \ check the boxes beside them. To remove game pieces,\
  \ uncheck their boxes. The two buttons on the bottom\
  \ left are \"Select All\" and \"Unselect All\" respectively.\
  \ To confirm a non-empty selection, click Ok."

txt_3_3_2 =
  "\n\
  \Game Events\n\
  \  >> Conditions\n\
  \    >> Key Combinations\n\
  \----------------------------\n\
  \\n\
     \\tThe key combinations editor is easy to deal with.\
  \ This condition is fulfilled if any of the specified\
  \ set of keys was pressed. Click the Add button to add\
  \ a new (empty) key entry. Select any key entry and\
  \ press a key combination to replace the one inside,\
  \ and click the Remove button to remove any unwanted\
  \ keys. To confirm a non-empty set of key combinations\
  \ (ignoring the blank ones), click Ok."

txt_3_3_3 =
  "\n\
  \Game Events\n\
  \  >> Conditions\n\
  \    >> Configurations\n\
  \------------------------\n\
  \\n\
     \\tA configuration is a game piece occupying some\
  \ or all of certain board positions. This condition\
  \ is fulfilled if any of the specified configurations\
  \ are reached. The configurations editor has two levels,\
  \ \"multiple manager\" and \"single editor\". The\
  \ \"multiple manager\" allows configurations to be\
  \ added, removed and edited. When a configuration is\
  \ added or edited, the \"single editor\" dialog appears,\
  \ displaying the empty board and prompting the selection\
  \ of positions. The two menus at the bottom are also\
  \ to be browsed and have their options chosen from.\
  \ To confirm the non-empty specifications of a configuration,\
  \ click Ok and the added/edited configuration will\
  \ be displayed. Click Ok at the \"multiple manager\"\
  \ level to confirm a non-empty set of configurations."

txt_3_4 =
  "\n\
  \Game Events\n\
  \  >> Actions\n\
  \---------------\n\
  \\n\
     \\tWhen an action is added, a dialog box similar to\
  \ the one seen when conditions are added appears. The\
  \ menu at the top has, as its options, the 5 types of\
  \ actions, and the contents of the lower half of the\
  \ dialog box are unique to the action being selected\
  \ in the menu. The first two actions are special; the\
  \ first is the Game Ends action, and the other action\
  \ gives the turn to the next player. Since there are\
  \ no specifications to make for these two actions, the\
  \ focus is on the remaining three.\n\
  \\n\
     \\tBecause there are many ways to fulfill a condition\
  \ (e.g. click on any of these positions ...), and actions\
  \ usually need to react to the stimulus that fulfilled\
  \ the condition but have no way to find out what the\
  \ stimulus was (e.g. a piece needs to appear at the\
  \ position that was clicked, which is still an unknown),\
  \ actions need to associate themselves with the conditions\
  \ inside the conditions tree. Hence, most of the action\
  \ editors include a reference to conditions, so users\
  \ can associate actions with them."

txt_3_4_1 =
  "\n\
  \Game Events\n\
  \  >> Actions\n\
  \    >> Game Piece Transformation\n\
  \   and Game Piece Disappearances\n\
  \----------------------------------------\n\
  \\n\
     \\tThe editor for both transformations and appearances\
  \ containes a condition reference window which displays\
  \ only conditions in the same pair as the action,\
  \ and which also contain game pieces. When a condition\
  \ is selected, the buttons on the top right can check\
  \ and uncheck the checkbox beside it (indicating if\
  \ it is referred to by the action). The button on\
  \ the top left views the game pieces contained by\
  \ a selected condition. For transformations, the menu\
  \ of game pieces at the bottom decides what the game\
  \ pieces transform into. Click Ok when the selection\
  \ is final and there is at least one reference."

txt_3_4_2 =
  "\n\
  \Game Events\n\
  \  >> Actions\n\
  \    >> Game Piece Appearances\n\
  \-------------------------------------\n\
  \\n\
     \\tWhat this action needs is board positions to\
  \ let game pieces appear on, so the condition reference\
  \ shows only conditions containing positions. The\
  \ checkbox feature worksd the same as the last section,\
  \ and the bottom-left \"magnifying glass\" button shows\
  \ the postions contained in a selected condition.\
  \ This time, there is the possibility of the action\
  \ not requiring a reference to a condition, so there is\
  \ the familiar board positions editor components in the\
  \ \"Absolute Position\" tab."