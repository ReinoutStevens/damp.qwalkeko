package qwalkeko;

import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.jdt.core.dom.CompilationUnit;
import org.eclipse.jdt.core.dom.ASTParser;
import org.eclipse.jdt.core.dom.AST;
import org.eclipse.jdt.core.dom.MethodDeclaration;



import damp.ekeko.JavaProjectModel;

public class QwalkekoProjectModel extends JavaProjectModel {

	private Collection<IFile> javaFiles = new ArrayList<IFile>();
	private Collection<CompilationUnit> cus = new ArrayList<CompilationUnit>();
	
	public QwalkekoProjectModel(IProject p) {
		super(p);
	}
	
	public void clean(){
		super.clean();
		javaFiles =  new ArrayList<IFile>();
		cus = new ArrayList<CompilationUnit>();
	}
	
		
	public void populate(IProgressMonitor monitor) throws CoreException{
		System.out.println("Populating QwalkekoProjectModel for: " + javaProject.getElementName());
		findAndAddJavaFiles();
		for (IFile javaFile : javaFiles) {
			IPath localPath = javaFile.getLocation();
			String osPath = javaFile.getRawLocation().toOSString();
			String contents;
			try {
				contents = readFileToString(osPath);
			} catch (IOException e) {
				// TODO Auto-generated catch block
				IStatus status = new Status(Status.ERROR, Activator.PLUGIN_ID, e.getMessage());
				throw new CoreException(status);
			}
			CompilationUnit unit = parse(contents, localPath);
			cus.add(unit);
		}
		
		//for (CompilationUnit compilationUnit : cus) {
		//	
		//	icu2ast.put((ICompilationUnit)compilationUnit.getJavaElement(),compilationUnit);
		//}
		
		gatherInformationFromCompilationUnits();
	}


	public void addFile(IPath path){
		javaFiles.add(ResourcesPlugin.getWorkspace().getRoot().getFile(path));	
	}
	
	private void findAndAddJavaFiles() throws CoreException{
		if(javaFiles.isEmpty()){
			for(IResource resource : getProject().members()){
				processResource(resource);
			}	
		}
	}
	

	private void processResource(IResource resource) throws CoreException{
		if (resource instanceof IFolder) {
			IFolder folder = (IFolder) resource;
			processFolder(folder);
		} else if (resource instanceof IFile) {
			IFile file = (IFile) resource;
			processFile(file);
		}
	}
	
	private void processFolder(IFolder folder) throws CoreException{
		IPath path = folder.getFullPath();
		if(!isHidden(path)){
			for(IResource resource : folder.members()){
				processResource(resource);
			}
		}
	}
	
	
	
	private void processFile(IFile file){
		IPath path = file.getFullPath();
		if("java".equals(path.getFileExtension())){
			addFile(path);
		}
	}
	
	
	private boolean isHidden(IPath path){
		String segment = path.lastSegment();
		if(segment == null){
			return false;
		}
		return segment.startsWith(".");
	}
	
	
	private CompilationUnit parse(String str, IPath location) {
		ASTParser parser = ASTParser.newParser(AST.JLS8);
		parser.setSource(str.toCharArray());
		parser.setKind(ASTParser.K_COMPILATION_UNIT);
		parser.setUnitName(location.toString());
		parser.setProject(getJavaProject());
		CompilationUnit cu = (CompilationUnit) parser.createAST(null);
		return cu;
	}
	
	private String readFileToString(String filePath) throws IOException {
		StringBuilder fileData = new StringBuilder(1000);
		BufferedReader reader = new BufferedReader(new FileReader(filePath));
 
		char[] buf = new char[10];
		int numRead = 0;
		while ((numRead = reader.read(buf)) != -1) {
			String readData = String.valueOf(buf, 0, numRead);
			fileData.append(readData);
			buf = new char[1024];
		}
 
		reader.close();
 
		return  fileData.toString();	
	}
	
	
	public Iterable<CompilationUnit> getCompilationUnits(){
		return cus;
	}

	protected void addControlFlowGraphInformationForMethodDeclaration(MethodDeclaration m) {
		//no
	}
}
