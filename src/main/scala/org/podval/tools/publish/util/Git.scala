package org.podval.tools.publish.util

import org.eclipse.jgit.api.Git
import org.eclipse.jgit.api.errors.GitAPIException
import org.eclipse.jgit.diff.DiffConfig
import org.eclipse.jgit.lib.{Constants, Repository}
import org.eclipse.jgit.revwalk.{FollowFilter, RevCommit, RevWalk}
import org.eclipse.jgit.storage.file.FileRepositoryBuilder

import java.io.{File, IOException}
import java.util.Date

final class Git(rootDirectory: File):
  private lazy val repository: Option[Repository] =
    val dotGit = File(rootDirectory, ".git")
    Option.when(dotGit.exists)(FileRepositoryBuilder().setGitDir(dotGit).build)

//  def modificationDate(filePath: String) = repository.map: repository =>
//    val revWalk: RevWalk = RevWalk(repository) // TODO release
//    revWalk.markStart(revWalk.parseCommit(repository.resolve(Constants.HEAD)))
//        revWalk.setRevFilter(FollowFilter.create(filePath, DiffConfig.KEY))
//        val lastCommit: RevCommit = revWalk.iterator.next
//        // Get commit time (seconds since epoch)
//        val commitTime: Int = lastCommit.getCommitTime


//    try (Git git = Git.open(repoDir)) {
//      Iterable < RevCommit > commits = git.log()
//        .addPath(filePath)
//        .setMaxCount(1) // Only fetch the most recent commit
//        .call();
//
//      for (RevCommit commit: commits) {
//        // Returns the Unix timestamp (seconds since epoch)
//        int commitTimeSeconds = commit.getCommitTime();
//        return new Date((long) commitTimeSeconds *
//        1000
//        );
//      }
//    }
//    return null; // File has no commit history in the repo
//  }
//}
    
    
    
//import org.eclipse.jgit.ignore.IgnoreNode;
//import org.eclipse.jgit.ignore.IgnoreNode.MatchResult;
//import java.io.File;
//import java.io.FileInputStream;
//import java.io.IOException;
//
//public class GitIgnoreParser {
//    public static void main(String[] args) {
//        File gitIgnoreFile = new File(".gitignore");
//        IgnoreNode ignoreNode = new IgnoreNode();
//
//        // 1. Parse the .gitignore file
//        try (FileInputStream fis = new FileInputStream(gitIgnoreFile)) {
//            ignoreNode.parse(fis);
//        } catch (IOException e) {
//            System.err.println("Error reading .gitignore: " + e.getMessage());
//            return;
//        }
//
//        // 2. Test file paths against the rules
//        // Note: Target paths must be relative to the directory containing the .gitignore file
//        String fileToCheck1 = "target/classes/Main.class";
//        String fileToCheck2 = "src/main/java/App.java";
//
//        System.out.println(fileToCheck1 + " ignored? " + isIgnored(ignoreNode, fileToCheck1, false));
//        System.out.println(fileToCheck2 + " ignored? " + isIgnored(ignoreNode, fileToCheck2, false));
//    }
//
//    /**
//     * Evaluates if a path matches the parsed gitignore rules.
//     * 
//     * @param ignoreNode Parsed rules container
//     * @param path Relative path to the file or directory
//     * @param isDirectory True if the path points to a directory
//     */
//    public static boolean isIgnored(IgnoreNode ignoreNode, String path, boolean isDirectory) {
//        MatchResult result = ignoreNode.isIgnored(path, isDirectory);
//        return result == MatchResult.IGNORED;
//    }
//}    