package qwalkeko.parser;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import org.eclipse.core.resources.IProject;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.Locator;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;
import org.xml.sax.helpers.XMLReaderFactory;

import qwalkeko.building.BuildCommand;
import qwalkeko.building.DeleteFolderCommand;
import qwalkeko.building.ProjectBuilder;

public class ProjectBuilderHandler extends DefaultHandler{

	private ProjectBuilder projectBuilder;
	

	private Locator locator;

	
	public static ProjectBuilderHandler parseFile(File file) throws IOException, SAXException{
		XMLReader xr = XMLReaderFactory.createXMLReader();
		ProjectBuilderHandler handler = new ProjectBuilderHandler();
		xr.setContentHandler(handler);
		xr.setErrorHandler(handler);
		FileReader fr = new FileReader(file);
		InputSource is = new InputSource(fr);
		xr.parse(is);
		return handler;
	}
	
	public ProjectBuilder getProjectBuilder() {
		return projectBuilder;
	}
	
	
	public void startDocument() throws SAXException{
		super.startDocument();
		
	}
	
	public void setDocumentLocator(Locator locator){
		super.setDocumentLocator(locator);
		this.locator = locator;
	}
	
	public void startElement(String uri, String name, String qName, Attributes atts) throws SAXException{
		super.startElement(uri, name, qName, atts);
		if(name.equals("build")){
			this.beginBuild(uri, name, qName, atts);
		} else if(name.equals("delete")){
			this.beginDelete(uri, name, qName, atts);
		}
		
	}

	private void beginDelete(String uri, String name, String qName,	Attributes atts) throws SAXException {
		if(projectBuilder == null){
			throw new SAXParseException("unexpected delete", locator);
		}
		int typeNameIdx = atts.getIndex(uri, "type");
		int resourceNameIdx = atts.getIndex(uri, "name");
		if(typeNameIdx == -1){
			throw new SAXParseException("missing type specification", locator);
		}
		if(resourceNameIdx == -1){
			throw new SAXParseException("missing name specification", locator);
		}
		String typeName = atts.getValue(typeNameIdx);
		String resourceName = atts.getValue(resourceNameIdx);
		BuildCommand command = null;
		if(typeName.equals("folder")){
			command = new DeleteFolderCommand(resourceName);
		}
		if(command == null){
			throw new SAXParseException("unknown type specification", locator);
		}
		projectBuilder.addCommand(command);
	}

	private void beginBuild(String uri, String name, String qName, Attributes atts) {
		projectBuilder = new ProjectBuilder();
	}
}
