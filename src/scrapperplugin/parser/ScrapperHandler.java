package scrapperplugin.parser;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import scrapperplugin.MetaProject;
import scrapperplugin.MetaVersion;

public class ScrapperHandler extends DefaultHandler {

	private Locator locator;
	private MetaProjectHandler currentProject;
	private MetaVersionHandler currentVersion;
	private MetaProject project;
	
	
	public static ScrapperHandler parseFile(File file) throws IOException, SAXException{
		XMLReader xr = XMLReaderFactory.createXMLReader();
		ScrapperHandler handler = new ScrapperHandler();
		xr.setContentHandler(handler);
		xr.setErrorHandler(handler);
		FileReader fr = new FileReader(file);
		InputSource is = new InputSource(fr);
		xr.parse(is);
		return handler;
	}
	
	
	public MetaProject getMetaProject(){
		return project;
	}
	
	
	
	public void startDocument() throws SAXException{
		super.startDocument();
		
	}
	
	public void setDocumentLocator(Locator locator){
		super.setDocumentLocator(locator);
		this.locator = locator;
	}
	
	public void startElement (String uri, String name, String qName, Attributes atts) throws SAXException{
		super.startElement(uri, name, qName, atts);
		if(name.equals("project")){
			this.beginProject(uri, name, qName, atts);
		} else if(name.equals("version")){
			this.beginVersion(uri, name, qName, atts);
		} else if(name.equals("predecessor")){
			this.beginPredecessor(uri, name, qName, atts);
		} else if(name.equals("successor")){
			this.beginSuccessor(uri, name, qName, atts);
		} else if(name.equals("time")){
			this.beginTime(uri, name, qName, atts);
		}
		
	}


	public void endElement (String uri, String name, String qName) throws SAXException{
		super.endElement(uri, name, qName);
		if(name.equals("project")){
			this.endProject();
		} else if(name.equals("version")){
			this.endVersion();
		}
	}
	
	
	private void beginProject(String uri, String name, String qName, Attributes atts) throws SAXException{
		if(currentProject != null){
			throw new SAXParseException("unexpected project", locator);
		}
		currentProject = new MetaProjectHandler();
		currentProject.parseProject(uri, name, qName, atts);
	}
	
	private void beginVersion(String uri, String name, String qName, Attributes atts) throws SAXException{
		if(currentVersion != null){
			throw new SAXParseException("unexpected version (perhaps it is spanish)", locator);
		}
		currentVersion = new MetaVersionHandler();
		currentVersion.parseVersion(uri, name, qName, atts);
		
		
	}
	
	
	private void beginPredecessor(String uri, String name, String qName, Attributes atts) throws SAXException{
		if(currentVersion == null){
			throw new SAXParseException("unexpected predecessor", locator);
		}
		currentVersion.parsePredecessor(uri, name, qName, atts);
	}
	
	private void beginSuccessor(String uri, String name, String qName, Attributes atts) throws SAXException{
		if(currentVersion == null){
			throw new SAXParseException("unexpected predecessor", locator);
		}
		currentVersion.parseSuccessor(uri, name, qName, atts);
	}

	private void beginTime(String uri, String name, String qName, Attributes atts) throws SAXException{
		if(currentVersion == null){
			throw new SAXParseException("unexpected time", locator);
		}
		currentVersion.parseTime(uri, name, qName, atts);
	}
	
	
	private void endProject() throws SAXException {
		this.project = currentProject.createProject();
		currentProject = null;
	}
	
	private void endVersion() throws SAXException {
		currentProject.addVersion(currentVersion.createVersion());
		currentVersion = null;
	}
	
}
