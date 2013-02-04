(* ::Package:: MSFTP` *)

(* ::Copyright:: Rolf Mertig, 2013 *)

(* ::Author:: Rolf Mertig , GluonVision GmbH,  http://www.mertig.com *)

(* ::License:: LGPL *)

(* ::Title:: *)
(* Implementation of sftp for Mathematica, using JLink and vngx-jsch *)

(* :: Mathematica Version: 7, 8, or 9 *)

(* ::Installation and loading:: *)
(*

Import["http://packageinstaller.googlecode.com/hg/PackageInstaller/PackageInstaller.m"];
PackageInstaller`InstallPackage["http://msftp.googlecode.com/hg/MSFTP.zip"];
Needs["MSFTP`"];
NotebookOpen[ FileNameJoin[{ParentDirectory@DirectoryName[FindFile["MSFTP`"]], "MSFTP.nb"}]];

*)


BeginPackage["MSFTP`", {"JLink`"}]

Unprotect[MSFTPPut, MSFTPGet, PassEncode];
ClearAll[MSFTPPut, MSFTPGet, PassEncode];

InstallJava[];

Block[{ifn},
	ifn = If[$VersionNumber < 8, System`Private`FindFile[$Input], $InputFileName];
 	AddToClassPath@FileNameJoin[{ DirectoryName @ ifn, "Java"}]
];

PassEncode::usage="PassEncode[secret] encodes the password secret for the $MachineID of the current computer. 
The resulting list can be given as an encoded password setting for the option Password of MSFTPPut and MSFTPGet.";

MSFTPGet::usage = "MSFTPGet[remotefile, localdir] transfers remotefile to localdir.";

MSFTPPut::usage = "MSFTPPut[fileordirectory, \"UserName\" -> \"rolfm\", \"HostName\" -> \"sftpserver.company.com\", 
\"Password\"->\"secret\"] uploads fileordirectory. MSFTPPut[fileordir, remotedir] uploads to remotedir.
The password can be either given in clear text, or as a list of integers as returned by PassEncode[\"mypassword\"].";

openchan::authenticated="Authentication failed for user `1` at `2`.";
openchan::connected="Connection failed for user `1` at `2`.";

Begin["`Private`"]

Options[MSFTPGet] = {
                     "HostName" -> "test.gluonvision.de", 
                     "Password" -> "secret",
                     "Port" -> 22,
                     (* there are more of these options. In principle easy to implement *)
                     "StrictHostKeyChecking" -> "no",
                     "PreferredAuthentications" -> "password",
                     "UserName" :> $UserName,
                     Print -> True
                     };
                     
Options[MSFTPPut] = Options[MSFTPGet];

PassEncode[pass_String] :=
    Module[ {tmp, enc, tmpfile},
    	tmpfile = OpenWrite[];
        tmp = Close @ Export[tmpfile, Hold @@ {pass}, "Text"];
        Encode[tmp, tmp <> ".enc", MachineID -> $MachineID];
        DeleteFile[tmp];
        enc = Import[tmp <> ".enc", "Text"];
        DeleteFile[tmp <> ".enc"];
        ToCharacterCode@enc
    ];
   
PassDecode[s_String] := s;  
   
PassDecode[li : {__Integer}] := ReleaseHold[ImportString[FromCharacterCode[li], "Package"]];

openChannel[prot_String, opts_List] :=
    Block[ {setProperty, connect, str2byte, isAuthenticated, isConnected, createSession},
        Catch[Module[ {channel, conf, jsch, util, username, host, port, pwd, session},
                  InstallJava[];
                  {jsch, util, conf} = 
                  JavaNew /@ {"org.vngx.jsch.JSch", "org.vngx.jsch.Util", "org.vngx.jsch.config.SessionConfig"};
                  conf[setProperty["StrictHostKeyChecking", "StrictHostKeyChecking" /. opts]];
                  conf[setProperty["PreferredAuthentications", "PreferredAuthentications" /. opts]];
                  {username, host, port, pwd} = {"UserName", "HostName", "Port", "Password"} /. opts;
                  session = jsch[createSession[username, host, port, conf]];
                  session[connect[util[str2byte[PassDecode[pwd]]]]];
                  If[ session[isAuthenticated[]] =!= True, Message[openchan::authenticated, username, host]; Throw[$Failed] ];
                  channel = session[openChannel[prot]];
                  channel[connect[]];
                  If[ channel[isConnected[]] === False, Message[openchan::connected, username, host]; Throw[$Failed] ];
                  channel
              ]]
    ]; 
            
(* get files *)
MSFTPGet[remotefile_String, localdir_String:Directory[], opts:OptionsPattern[] ] :=
Block[{getAttrs, getSize, toArray, get, disconnect, getFilename, ls},
    Catch @ Module[ {channel, lsfile, filesize, remotefiles, lsrem, localfilenames, print},
    	If[OptionValue[Print]===Print, print = Print, print = Hold];
			channel = openChannel["sftp", Flatten[Join[{opts}, Options[MSFTPGet]]]];
              lsrem = channel @ ls[remotefile];
              If[ lsfile === $Failed, Message[MSFTP::notfound, remotefile]; Throw[$Failed] ];
              (* even though not useful here, this would also work with a pattern *)
              filesize = Total@Through[Through[lsrem@toArray[]@getAttrs[]]@getSize[]];
              If[ OptionValue[Print],
                  Print["# of bytes to transfer : ", filesize]
              ];
              remotefiles = Through[lsrem@toArray[]@getFilename[]];
              localfilenames = FileNameJoin[{localdir, #}]& /@ remotefiles;
              Do[ 
              	print["transferring ", remotefiles[[i]]];
              	channel@get[remotefiles[[i]], localfilenames[[i]]], {i, Length@remotefiles}];
              channel@disconnect[];
              Total[ FileByteCount /@ localfilenames ]
    	]]

(* local can be either a file, or a wildcard or a directory *)
(*  If no second argument is given then the root directory of the sftp server will be used *)
MSFTPPut[local_String, opts:OptionsPattern[]] :=
    MSFTPPut[local, ".", opts];
    
putfile[chan_, file_String?FileExistsQ] := Block[{put}, Module[{filesize, flocal},
                  filesize = FileByteCount @ file;
                  If[ OptionValue[Print], Print["# of bytes to transfer : ", filesize] ];
                  flocal = FileNameTake[file];
                  chan @ put[file, flocal]; 
                  ]];
                  
(* if remote directory r does not exist, create it and cd to it *)
chdir[chan_, r_String /; r =!= "."] := Block[{lsrem, ls, mkdir, cd},
              	Quiet[lsrem = chan @ ls[r]]; 
              	      If[ lsrem === $Failed, chan @ mkdir[r]]; 
              	      chan @ cd[r]; ];
              	
putdir[chan_, dir_String?DirectoryQ, opts___?OptionQ] := Block[{cd, put, pwd}, 
	Module[{dirfiles, files, rdir},
				  SetDirectory[dir];
				  rdir = chan @ pwd[];
				  chdir[chan, FileNameTake @ dir];
				  (* all files and dirs in that dir *)
				  dirfiles = FileNames["*", dir];
				  (* transfer all files first *)
				  files = Select[dirfiles, FileType[#] === File&];
				  (* if there is a problem with big files it originates here: *)
				  Scan[ putfile[chan, #]&, files];
				  (*
				  putfile[chan, #]& /@ Select[dirfiles, FileType[#] === File&];
				  *)
				  (* recurse *)
				  putdir[chan, #, opts]& /@ Select[dirfiles, FileType[#] === Directory&];
				  chan @ cd[rdir];
]];
                  
    
MSFTPPut[local_String, remotedir_String, opts:OptionsPattern[] ] :=
Block[{cd, getAttrs, getSize, toArray, get, disconnect, getFilename, mkdir, setProperty, ls, put},
    Catch@Module[ {channel, dir = Directory[]},
			channel = openChannel["sftp", Flatten[Join[{opts}, Options[MSFTPPut]]]];
			(*
Global`CHANNEL = channel;		
*)
			  chdir[channel, remotedir];
              If[ FileType[local] === File, putfile[channel, local]];
              If[ FileType[local] === Directory, putdir[channel, local, opts]];
              channel@disconnect[];
              SetDirectory[dir];
              local (* return the directory being uploaded *)
          ]]
          
          
SetAttributes[{MSFTPPut, MSFTPGet, PassEncode}, ReadProtected];
Protect[MSFTPPut, MSFTPGet, PassEncode];

End[]

EndPackage[]
