package qwalkeko.parser;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Collection;

import org.eclipse.core.runtime.CoreException;
import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import qwalkeko.MetaProject;
import qwalkeko.MetaVersion;


public class MetaProjectHandler {
	private String name;
	private URI uri;
	private Collection<MetaVersion> versions;
	
	
	public MetaProjectHandler(){
		this.versions = new ArrayList<MetaVersion>();
	}
	
	public void parseProject(String uri, String name, String qName, Attributes atts) throws SAXException{
		int nameIdx = atts.getIndex(uri, "name");
		if(nameIdx == -1){
			throw new SAXException("missing name attribute for project");
		}
		
		this.name = atts.getValue(nameIdx);
		
	}
	
	
	public void parseRepository(String uri, String name, String qName, Attributes atts) throws SAXException{
		int urlIdx = atts.getIndex(uri, "url");
		if(urlIdx == -1){
			throw new SAXException("missing url attribute for project");
		}
		try {
			this.uri = new URI(atts.getValue(urlIdx));
		} catch (URISyntaxException e) {
			throw new SAXException(e);
		}
	}
	
	public void addVersion(MetaVersion version){
		versions.add(version);
	}
	
	public MetaProject createProject() throws SAXException{
		if(name == null || uri == null){
			throw new SAXException("invalid project");
		}
		MetaProject project = new MetaProject(name, uri);
		project.addVersions(versions);
		try{
			project.initialize();
		} catch(CoreException e){
			throw new SAXException(e);
		}
		return project;
	}
}
