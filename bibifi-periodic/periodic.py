#!/usr/bin/env python
import sys
import signal
import hashlib
import select
import pickle
import os
import git
import datetime
import base64
import logging
import imp
import tempfile
import time
import json
import argparse
import random
import shutil
import traceback
import xml.etree.ElementTree as ET
from subprocess import Popen
from subprocess import PIPE
from subprocess import call
from threading import Thread
from time import sleep
from glob import glob

"""
Constant data
"""
#TRANSLATOR_PATH="./dist/build/translator/translator"
TRANSLATOR_PATH=".stack-work/dist/x86_64-linux/Cabal-1.22.5.0/build/translator/translator"
#TRANSLATOR_PATH="/mock_translator"

TRANSLATOR_CWD="../bibifi-code/bibifi-translator/"
#TRANSLATOR_CWD="../dev/translator/"
REPO_BASE="./repos/"
ARCHIVE_BASE="./archives/"
BREAKS_BASE = "./breaks/"

RUNNER_TOOL_PATH = "./ec2-runner/run.sh"

#             codepath teamname commithash currentphase
BUILD_TEST_CMDLINE = "%s %s %s %s"
#             unused        teamname commithash currentphase usertest
BREAK_TEST_CMDLINE = "unused %s %s break %s"

"""
Classes and types
"""

class TeamData:
  def __init__(self, teamName, teamURL):
    self.teamName = teamName
    self.teamURL = teamURL
    #make a git.Repo object for teamURL...
    self.repo = None
    self.lastCommitRun = None
    self.testingInProgress = False
    self.testingCommitHash = ""
    self.testingId = ""
    if len(self.teamURL) > 0:
      t = False
      if len(self.teamURL) > 17 and (self.teamURL[0:17] == "git@bitbucket.org" or self.teamURL[0:14] == "git@github.com" or self.teamURL[0:14] == "git@gitlab.com"):
        if os.path.isdir(REPO_BASE+teamName):
          self.repo = git.Repo(REPO_BASE+teamName)
          if self.repo.remotes[0].url != self.teamURL:
            path = REPO_BASE+"repo_old_%s_%d" % (teamName,0)
            k = 0
            while os.path.isdir(path) == True:
              k = k + 1
              path = REPO_BASE+"repo_old_%s_%d" % (teamName,k)
            shutil.move(REPO_BASE+teamName, "repo_old_%s_%d" % (teamName,k))
            try:
              self.repo = git.Repo.clone_from(self.teamURL, REPO_BASE+teamName)
            except git.exc.GitCommandError:
              pass
        else:
          try:
            self.repo = git.Repo.clone_from(self.teamURL, REPO_BASE+teamName)
          except git.exc.GitCommandError:
            logging.info( "Could not clone repo")
            pass
    return

  def __str__(self):
    return str(self.teamName)+" "+str(self.repo)+" "

  def __repr__(self):
    return self.__str__()

  def canTestRepo(self):
    return self.repo != None and len(self.repo.remotes) != 0 and len(self.repo.heads) != 0

  def newRefreshRepo(self, testedset):
    """
    Seriously, all software is terrible
    """
    done = False
    changes = None
    hashS = ""
    while done == False:
      try:
        # Some extra clean up. Hopefully it prevents hangs. 
        self.repo.remotes[0].fetch()
        # logging.info( "HERE: %s" % type(self.repo).__name__)
        self.repo.git.reset( '--hard', 'FETCH_HEAD')
        self.repo.git.clean('-df')
        # tmp = self.repo.remotes[0].pull()
        tmp = self.repo.commit('master')

        if ("commit:%s" % tmp.hexsha) not in testedset:
          changes = tmp
          hashS = changes.hexsha

        done = True
      except:
        done = False
    return changes,hashS

  """
  def refreshRepo(self):
    # Seriously, all software is terrible
    done = False
    changes = None
    hashS = ""
    while done == False:
      try:
        tmp = self.repo.remotes[0].pull()

        if len(tmp) != 0:
          changes = tmp[0]
          hashS = changes.commit.hexsha

        done = True
      except:
        done = False
    return changes,hashS
  """

  def getLatestCommitTime(self):
    """
    Read commit information from self.repo
    """
    done = False
    d = None
    while done == False:
      try:
        d = self.repo.heads[0].commit.committed_date
        done = True
      except:
        done = False
    return d
  
  def getSpecificCommitToUpload(self,s,t):
    """
    """
    if self.canTestRepo():
      b = "%s/%s" % (ARCHIVE_BASE, self.teamName)
      try:
        os.mkdir(b)
      except:
        pass
      p = "%s/%s.tar.gz" % (b, s)
      f = file(p, "w")
      self.repo.archive(f,treeish=t)
      f.close()
      return p
    else:
      return None

  def getCommitToUpload(self,s):
    """
    """
    if self.canTestRepo():
      b = "%s/%s" % (ARCHIVE_BASE, self.teamName)
      try:
        os.mkdir(b)
      except:
        pass
      p = "%s/%s.tar.gz" % (b, s)
      f = file(p, "w")
      self.repo.archive(f)
      f.close()
      return p
    else:
      return None

"""
Helper functions
"""
def parse_team_list(s):
  elements = []
  j = []
  try:
    j = json.loads(s)
  except:
    j = []
    logging.error("parse_team_list %s" % s)
  for i in j:
    elements.append(TeamData(str(i['teamId']), i['gitURL']))
  return elements

def get_teams_from_db():
  args = []
  args.append(TRANSLATOR_CWD+TRANSLATOR_PATH)
  args.append("RETRIEVE")
  args.append("TEAMS")
  p = Popen(args, stdout=PIPE, stderr=PIPE, cwd=TRANSLATOR_CWD)
  stdout, stderr = p.communicate()
  logging.debug("requests teams from database, got %s %s" % (stdout, stderr))
  return parse_team_list(stdout)

def get_teams_from_db2():
  args = []
  args.append(TRANSLATOR_PATH)
  args.append("RETRIEVE")
  args.append("TEAMS")
  try:
    p = Popen(args, stdout=PIPE, stderr=PIPE, cwd=TRANSLATOR_CWD)
  except Exception as e:
    logging.info("failed executing %s with: %s" % (str(args),str(e)))
    return get_teams_from_db2()
  stdout, stderr = p.communicate()
  logging.debug("requests teams from database, got %s %s" % (stdout, stderr))
  p = []

  try:
    p = json.loads(stdout)
  except:
    p = []
    logging.error("out == %s err == %s" % (stdout,stderr))

  return p

def changes_change_path(changes, path):
  #get a diff
  hcommit = changes.commit
  try:
    tcommit = hcommit.diff('HEAD~1')
  except:
    return False
  for diff in tcommit:
    if diff.a_blob != None:
      p = diff.a_blob.path
      if len(p) > len(path) and p[0:len(path)] == path:
        return True
  return False

def user_tests_from_change(teamName):
  tests = []
  breakdirname = "./repos/%s/break" % teamName
  if os.path.isdir(breakdirname):
    for g in glob(breakdirname+"/*"):
      if os.path.isdir( g):
        # print(g)
        # with open(g, 'rb') as f:
        hsh = hashlib.sha1(g).hexdigest()
        tests.append((g,hsh))
        # print(g)

  return tests

"""
Application logic
"""
dead = False

"""
def test_team_build(teamdata, lastdatetime):
  #logging.info("test_team_build")
  #do a pull on teamdata
  if teamdata.canTestRepo() == False:
    return lastdatetime
  changes,hexS = teamdata.refreshRepo()
  nextdatetime = lastdatetime
  if changes != None:
    logging.info("changes for team %s %s" % (teamdata.teamName, str(changes)))
  if changes != None:
    d = teamdata.getLatestCommitTime()
    builddatetime = datetime.datetime.fromtimestamp(d)
    logging.info("got new commit team %s on %s" % (teamdata.teamName, str(builddatetime)))
    if builddatetime <= lastdatetime or builddatetime > datetime.datetime(2014, 9, 10, 10, 0):
      return lastdatetime
    else:
      nextdatetime = builddatetime
    #okay, tar it and test it 
    committmp = teamdata.getCommitToUpload()
    #execute the thingy with the args 
    args = []
    args.append(RUNNER_TOOL_PATH)
    args.extend((BUILD_TEST_CMDLINE % (committmp, teamdata.teamName, hexS, "build")).split(" "))
    logging.info("going to execute %s" % str(args))
    f = file('logs/%s_%s.log' % (teamdata.teamName, hexS), 'w')
    ff = file('logs/%s_%s.err' % (teamdata.teamName, hexS), 'w')
    sleep(random.randint(0,20))
    p = Popen(args, stdout=f, stderr=ff)
    stdout,stderr = p.communicate()
    logging.info("output %s\nerror %s\n" % (stdout, stderr))
    try:
      os.unlink(committmp)
    except:
      pass
  return nextdatetime
"""

def test_team_generic(teamdata,testedset):
  if teamdata.canTestRepo() == False:
    return False
  changes,hexS = teamdata.newRefreshRepo( testedset)
  if changes != None:
    # d = teamdata.getLatestCommitTime()
    d = int( time.time())
    if ("commit:%s" % hexS) in testedset:
      return False
    testedset.add("commit:%s" % hexS)
    committmp = teamdata.getCommitToUpload(hexS)
    args = []
    args.append(TRANSLATOR_PATH)
    args.append("request")
    args.append("round1")
    args.append(teamdata.teamName)
    args.append("%s" % d)
    args.append(hexS)
    p = Popen(args, stdout=PIPE, stderr=PIPE, cwd=TRANSLATOR_CWD)
    p.communicate()
    return True
  return False

def test_team_break(teamdata,testedset):
  if teamdata.canTestRepo() == False:
    logging.info( "Cannot test repo for: %s" % teamdata.teamName)
    return False
  changes,hexS = teamdata.newRefreshRepo( testedset)
  if changes != None:
    # print( "hexS: %s" % hexS)
    if ("commit:%s" % hexS) in testedset:
      return False
    testedset.add("commit:%s" % hexS)
    # d = teamdata.getLatestCommitTime()
    d = int( time.time())
    tsts = user_tests_from_change(teamdata.teamName)
    # print( str(len(tsts)))
    #tsts: list of (path,hash of path) to break dir
    for (testdirpath,testhash) in tsts:
      # print("not in set: %s" % str(testhash not in testedset))
      # print("testpath: %s" % testpath)
      testname = os.path.basename(os.path.normpath(testdirpath))
      testnameFile = ".teamdata%s.%s" % (teamdata.teamName, testname)
      if not os.path.isfile(testnameFile):
      # if testhash not in testedset:
        # print("testpath: %s" % testpath)
        # print("testhash: %s" % testhash)
        # print("testedset: %s" % str(testedset))
        testpath = os.path.normpath(testdirpath) + "/test.json"
        go = False
        targetTeam = ""
        try:
          j = json.load(file(testpath, 'r'))
          targetTeam = str(j['target_team'])
          go = True
        except:
          logging.debug("Invalid: %s" % testpath)
          pass
        if go == True:
          # print("***here***")
          # Zip up test.
          zipdir = "%s%s" % (BREAKS_BASE, teamdata.teamName)
          try:
            os.mkdir( zipdir)
          except:
            pass
          zippath = "%s/%s" % (zipdir, testname)
          shutil.make_archive(zippath, 'zip', testdirpath)
          # testedset.add(testhash)
          #push to database
          args = []
          args.append(TRANSLATOR_PATH)
          args.append("request")
          args.append("round2")
          args.append(teamdata.teamName)
          args.append(targetTeam)
          args.append("%s" % d)
          args.append(hexS)
          args.append(testname)
          p = Popen(args, stdout=PIPE, stderr=PIPE, cwd=TRANSLATOR_CWD)
          p.communicate()
          # print("***here2***")
          open(testnameFile, 'a').close() # touch file
    return True

def fixes_from_change(teamName):
  fixes = []
  fixdirname = "./repos/%s/fix" % teamName
  for g in glob(fixdirname+"/*"):
    if os.path.isdir( g):
      fixes.append(g)
  return fixes

def parse_fix_json(path):
  n = None
  try:
    n = json.load(file(path))
  except ValueError:
    pass
  except IOError:
    pass
  if n != None:
    if n.has_key('commit') and n.has_key('breaks'):
      c = n['commit']
      d = n['breaks']
      if isinstance(d, list):
        return (c,[str(i) for i in d])
  return None

def parse_fix_xml(xmlpath):
  t = None
  try:
    t = ET.parse(xmlpath)
  except:
    pass
  if t == None:
    return None
  #get the commithash element
  commithash = None
  comm = t.findall("commit")
  for c in comm:
    if c.tag == "commit":
      commithash = c.text
  #get the breakid element
  breakids = []
  comm = t.findall("breakid")
  for c in comm:
    if c.tag == "breakid":
      breakids.append(c.text)
  #return them as a pair if they are both present
  if commithash != None and len(breakids) > 0:
    return (commithash,breakids)
  return None

def test_team_fix(teamdata, testedset):
  if teamdata.canTestRepo() == False:
    logging.info( "Cannot test repo for: %s" % teamdata.teamName)
    return False
  # changes,hexS = teamdata.refreshRepo()
  changes,hexS = teamdata.newRefreshRepo( testedset)
  # d = teamdata.getLatestCommitTime()
  # fixtime = datetime.datetime.fromtimestamp(d)
  d = int( time.time())
  fixes = fixes_from_change(teamdata.teamName)
  changed = False
  for fix in fixes:
    nm = os.path.basename(os.path.normpath( fix))
    fixid = teamdata.teamName+"_"+nm
    # ext = os.path.splitext(fix)[1]
    if nm != 'code' and fixid not in testedset: # ext == ".json" and fixid not in testedset:
      changed = True
      n = parse_fix_json(fix+"/fix.json")
      if n != None:
        #get the commit hash of the fix and the ID of the test it fixes
        commithash,breakids = n
        #do an archive at the point of the fix
        if not os.path.isdir(ARCHIVE_BASE+"/"+teamdata.teamName):
          os.mkdir(ARCHIVE_BASE+"/"+teamdata.teamName)
        pth = "%s/%s/%s.tar.gz" % (ARCHIVE_BASE,teamdata.teamName,commithash)
        f = file(pth, 'w')
        try:
          teamdata.repo.archive(f, treeish=commithash)
        except git.GitCommandError:
          print "someone gave us a nonexistant treeish"
          return False
        f.close()
        #report the current thing
        # nm = os.path.basename(os.path.splitext(fix)[0])
        # args = "%s Request ROUND3 %s %d %s %s %s %s" % (TRANSLATOR_PATH, teamdata.teamName, d, commithash, "0", nm, " ".join(breakids))
        args = []
        args.append(TRANSLATOR_PATH)
        args.append("request")
        args.append("ROUND3")
        args.append(teamdata.teamName)
        args.append( d)
        args.append( commithash)
        args.append( "0")
        args.append( nm)
        args.extend( breakids)
        p = Popen(args, stdout=PIPE, stderr=PIPE, cwd=TRANSLATOR_CWD)
        stdout,stderr = p.communicate()
        logging.debug("called %s got %s %s" % (args, stdout, stderr))
        testedset.add(fixid)

  return changed

def test_team_loop(teamdata):
  global dead
  curteam = teamdata
  curdatetime = datetime.datetime(2014, 9, 1, 0, 0)
  f = None
  try:
    f = file(".teamdata%s" % curteam.teamName, 'r')
  except:
    pass
  testedset = set()
  if f != None:
    testedset = pickle.load(f)
    f.close()
  while dead == False:
    # logging.info( "another round %s" % dead)
    fromdb = get_teams_from_db2()
    gitURL = ""
    for t in fromdb:
      teamName = str(t['teamId'])
      gitURL = t['gitURL']
      if teamName == curteam.teamName and gitURL != curteam.teamURL:
        curteam = TeamData(teamName, gitURL)
        break
    #old?? curdatetime = test_team_build(curteam, curdatetime)

    changed = test_team_generic(curteam, testedset)
    #changed = test_team_break(curteam, testedset)
    #changed = test_team_fix(curteam, testedset)

    if changed:
      f = file(".teamdata%s" % curteam.teamName, 'w')
      pickle.dump(testedset, f)
      f.close()
    time.sleep(40)
  return

threads = []
def main():
  global threads
  logging.basicConfig(filename="periodic.log", level=logging.INFO)
  testing = set()
  signal.signal( signal.SIGINT, exit_handler)
  while True:
    teams = get_teams_from_db2()
    for team in teams:
      teamName = str(team['teamId'])
      gitURL = team['gitURL']
      # Check if this team already has a thread testing it.
      if teamName not in testing:
        testing.add( teamName)
        logging.info("creating new thread for team %s" % teamName)
        #make a thread per team
        t = Thread(target=test_team_loop, args=( TeamData(teamName, gitURL),))
        t.start()
        threads.append(t)

    time.sleep(20)

  
  return 

def exiting_handler(s, f):
  print("Already exiting...")
  waitToDie()

def exit_handler(s, f):
  global dead

  dead = True
  signal.signal( signal.SIGINT, exiting_handler)
  print("Will exit once all threads are done.")
  waitToDie()

def waitToDie():
  global threads

  #wait for this to die
  for t in threads:
    if t.isAlive() == True:
      t.join(1)
  sys.exit(0)

if __name__ == '__main__':
  main()
