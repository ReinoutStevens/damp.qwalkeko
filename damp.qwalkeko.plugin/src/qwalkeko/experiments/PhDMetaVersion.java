package qwalkeko.experiments;

import java.io.File;
import java.io.IOException;
import java.util.Calendar;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jgit.lib.ObjectId;
import org.eclipse.jgit.revwalk.RevCommit;

import damp.ekeko.EkekoNature;
import qwalkeko.Activator;
import qwalkeko.GitCommands;
import qwalkeko.MetaVersion;
import qwalkeko.QwalkekoNature;
import qwalkeko.building.ProjectBuilder;

public class PhDMetaVersion {

	private static String repoDir = "/Users/resteven/Documents/PhD/repositories/";
	RevCommit revCommit;
	String gitRepo;
	private IProject eclipseProject;

	
	public PhDMetaVersion(RevCommit revCommit, String gitRepo) {
		this.revCommit = revCommit;
		this.gitRepo = gitRepo;
	}

	private String getVersionRepositoryLocation(){
		return gitRepo + "-" + getRevisionNumber();
	}
	
	public void openAndCheckoutIfNeeded() throws CoreException{
		File root = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile();
		File targetLocation = new File(root, getVersionRepositoryLocation());
		try{
			if(!targetLocation.exists()){
				File sourceLocation = new File(repoDir, gitRepo);
				doCloneOperation(sourceLocation, targetLocation);
				doCheckoutOperation();
				//eclipse does silly things when a .git is present, especially when you delete that folder later
				deleteGitFolder();
			} 
		} catch(Exception e){
			IStatus status = new Status(Status.ERROR, Activator.PLUGIN_ID, e.getMessage(), e);
			throw new CoreException(status);
		}
		if(eclipseProject == null || !eclipseProject.exists()){
			createEclipseMetaProject();
		}
	}
	
	public void closeAndDeleteIfNeeded() throws CoreException{
		if(eclipseProject == null){
			String loc = getVersionRepositoryLocation();
			eclipseProject = ResourcesPlugin.getWorkspace().getRoot().getProject(loc);
		}
		if(eclipseProject != null){
			damp.util.Natures.removeNature(eclipseProject, EkekoNature.NATURE_ID);
			eclipseProject.delete(true, false, null);
			eclipseProject = null;
		}
	}
	
	private void doCloneOperation(File sourceLocation, File targetLocation) throws IOException, InterruptedException{
		assert(sourceLocation.exists());
		assert(!targetLocation.exists());
		String[] cmd = { GitCommands.gitCommand(), "clone", "--shared", sourceLocation.getAbsolutePath(), targetLocation.getAbsolutePath() };
		Process proc = Runtime.getRuntime().exec(cmd);
		if(proc.waitFor() != 0){
			assert(false);
		}
	}
	
	private String getRevisionNumber(){
		return ObjectId.toString(revCommit.getId());
	}
	
	private void doCheckoutOperation() throws IOException, InterruptedException{
		/*(Repository repo = versionRepository.getRepository();
		ObjectId head = repo.resolve(Constants.HEAD);
		Iterable<RevCommit> commits = versionRepository.log().add(head).call();
		RevCommit commit = null;
		for(RevCommit c : commits){
			if(c.getId().getName().equals(this.getRevisionNumber())){
				commit = c;
				break;
			}
		}
		versionRepository.checkout().setStartPoint(commit).setAllPaths(true).call();
		*/
		File root = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile();
		File targetLocation = new File(root, getVersionRepositoryLocation());
		String[] cmd = { GitCommands.gitCommand(), "checkout", getRevisionNumber() };
		Process proc = Runtime.getRuntime().exec(cmd, null, targetLocation);
		if(proc.waitFor() != 0){
			assert(false);
		}
	}
	
	private void createEclipseMetaProject(){
		String loc = getVersionRepositoryLocation();
		eclipseProject = ResourcesPlugin.getWorkspace().getRoot().getProject(loc);
		//we get errors here, dont know why...
		try{
			if(!eclipseProject.exists()){
				eclipseProject.create(null);
			}
			if(!eclipseProject.isOpen()){
				eclipseProject.open(null);
			}
			ProjectBuilder builder = new ProjectBuilder();
			builder.build(eclipseProject);
			//always add ekeko nature last as the model starts building once the nature is added
			//some natures modify how ekeko builds its model
			/*String[] natures = new String[]{ QwalkekoNature.NATURE_ID, EkekoNature.NATURE_ID };
			for (int i = 0; i < natures.length; i++) {
				String nature = natures[i];
				if(!eclipseProject.hasNature(nature)){
					damp.util.Natures.addNature(eclipseProject, nature);
				}
			}*/
			try {
				Job.getJobManager().join(ResourcesPlugin.FAMILY_AUTO_BUILD, null);
			} catch (OperationCanceledException | InterruptedException e) {
				e.printStackTrace();
				return;
			}
		} catch(CoreException e){
			e.printStackTrace();
		}
	}
	private void deleteGitFolder() {
		File root = ResourcesPlugin.getWorkspace().getRoot().getLocation().toFile();
		File targetLocation = new File(root, getVersionRepositoryLocation());
		File gitLocation = new File(targetLocation, ".git");
		deleteDir(gitLocation);
	}
	
	private boolean deleteDir(File dir) { 
		  if (dir.isDirectory()) { 
			  String[] children = dir.list(); 
			  for (int i=0; i<children.length; i++) { 
				  boolean success = deleteDir(new File(dir, children[i])); 
				  if (!success) {  
					  return false; 
				  } 
			  }
		  }	
		  return dir.delete(); 
		}
}
