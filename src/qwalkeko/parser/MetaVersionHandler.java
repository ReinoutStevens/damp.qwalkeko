package qwalkeko.parser;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TreeSet;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import qwalkeko.ChangedFileInfo;
import qwalkeko.MetaVersion;
import qwalkeko.ChangedFileInfo.Status;


public class MetaVersionHandler {
	Calendar time = new GregorianCalendar();
	private String commitMessage;
	private String author;
	private String revision;
	
	private Collection<String> successors = new TreeSet<String>();
	private Collection<String> predecessors = new TreeSet<String>();
	
	private Collection<ChangedFileInfo> changedFiles = new ArrayList<ChangedFileInfo>();

	
	
	
	
	public void parseVersion (String uri, String name, String qName, Attributes atts) throws SAXException{
		assert(name.equals("version"));
		
		int revisionIdx = atts.getIndex(uri, "revision");
		int messageIdx = atts.getIndex(uri, "message");
		int authorIdx = atts.getIndex(uri, "author");
		int timeIdx = atts.getIndex(uri, "time");
		
		if(revisionIdx == -1){
			throwException("revision number");
		}
		if(messageIdx == -1){
			throwException("commitmessage");
		}
		if(authorIdx == -1){
			throwException("author");
		}
		if(timeIdx == -1){
			throwException("time");
		}
		
		this.revision = atts.getValue(revisionIdx);
		this.commitMessage = atts.getValue(messageIdx);
		this.author = atts.getValue(authorIdx);
		String dateString = atts.getValue(timeIdx);
		SimpleDateFormat format = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss");
		Date date;
		try {
			date = format.parse(dateString);
		} catch (ParseException e) {
			throw new SAXException(e);
		}
		Calendar.getInstance();	
		this.time.setTime(date);
	}
	
	
	
	public void parseSuccessor(String uri, String name, String qName, Attributes atts) throws SAXException{
		int revIdx = atts.getIndex(uri, "revision");
		if(revIdx == -1){
			throwException("successor number");
		}	
		String revNo = atts.getValue(revIdx);
		successors.add(revNo);
	}
	
	public void parsePredecessor(String uri, String name, String qName, Attributes atts) throws SAXException{
		int revIdx = atts.getIndex(uri, "revision");
		if(revIdx == -1){
			throwException("predecessor number");
		}	
		String revNo = atts.getValue(revIdx);
		predecessors.add(revNo);
	}
	
	
	public void parseChangedFile(String uri, String name, String qName, Attributes atts) throws SAXException{
		int fileIdx = atts.getIndex(uri, "file");
		int typeIdx = atts.getIndex(uri, "type");
		if(fileIdx == -1){
			throwException("file");
		}	
		if(fileIdx == -1){
			throwException("type");
		}
		String fileName = atts.getValue(fileIdx);
		ChangedFileInfo.Status status = Status.valueOf(atts.getValue(typeIdx).toUpperCase());
		changedFiles.add(new ChangedFileInfo(fileName, status));
	}
	
	public MetaVersion createVersion() throws SAXException{ 
		if(revision == null || commitMessage == null || author == null || time == null){
			throw new SAXException("invalid version");
		}
		MetaVersion version = new MetaVersion(revision, commitMessage, author, time);
		version.addSuccessors(successors);
		version.addPredecessors(predecessors);
		version.addChangedFiles(changedFiles);
		return version;
		
	}
	
	
	private void throwException(String missingAttribute) throws SAXException{
		
		throw new SAXException("missing " + missingAttribute);
	}
}
