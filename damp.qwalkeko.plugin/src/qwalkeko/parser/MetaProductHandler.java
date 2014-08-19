package qwalkeko.parser;

import java.util.ArrayList;
import java.util.Collection;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;

import qwalkeko.MetaProduct;
import qwalkeko.MetaProject;

public class MetaProductHandler {
	private Collection<MetaProject> metaProjects;
	
	public MetaProductHandler(){
		this.metaProjects = new ArrayList<MetaProject>();
	}
	
	public void parseProduct(String uri, String name, String qName, Attributes atts) throws SAXException{
		//nothing, hooray
	}
	
	public void addProject(MetaProject aProject){
		metaProjects.add(aProject);
	}
	
	public MetaProduct createMetaProduct(){
		MetaProduct result = new MetaProduct(metaProjects);
		return result;
	}
}
