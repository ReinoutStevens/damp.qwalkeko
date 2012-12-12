package qwalkeko.parser;

import java.io.File;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Collection;
import java.util.Date;
import java.util.GregorianCalendar;
import java.util.TreeSet;

import org.xml.sax.Attributes;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import qwalkeko.ChangedFileInfo;
import qwalkeko.MetaVersion;
import qwalkeko.ChangedFileInfo.Status;


public class MetaVersionHandler {

	Calendar time;
	private String commitMessage;
	private String author;
	private String revision;
	
	private Collection<String> successors = new TreeSet<String>();
	private Collection<String> predecessors = new TreeSet<String>();
	
	private Collection<ChangedFileInfo> changedFiles = new ArrayList<ChangedFileInfo>();

	
	
	
	
	public void parseVersion (String uri, String name, String qName, Attributes atts) throws SAXException{
		assert(name.equals("version"));
		
		int revisionIdx = atts.getIndex(uri, "revision");
		int commitIdx = atts.getIndex(uri, "commit");
		int authorIdx = atts.getIndex(uri, "author");
		if(revisionIdx == -1){
			throwException("revision number");
		}
		if(commitIdx == -1){
			throwException("commitmessage");
		}
		if(authorIdx == -1){
			throwException("author");
		}
		
		this.revision = atts.getValue(revisionIdx);
		this.commitMessage = atts.getValue(commitIdx);
		this.author = atts.getValue(authorIdx);
	}
	
	public void parseTime (String uri, String name, String qName, Attributes atts) throws SAXException{
		int dayIdx = atts.getIndex(uri, "day");
		int monthIdx = atts.getIndex(uri, "month");
		int secIdx = atts.getIndex(uri, "sec");
		int yearIdx = atts.getIndex(uri, "year");
		int hourIdx = atts.getIndex(uri, "hour");
		int minIdx = atts.getIndex(uri, "min");
		if(dayIdx == -1){
			throwException("day");
		}
		if(monthIdx == -1){
			throwException("month");
		}
		if(secIdx == -1){
			throwException("sec");
		}
		if(yearIdx == -1){
			throwException("year");
		}
		if(hourIdx == -1){
			throwException("hour");
		}
		if(minIdx == -1){
			throwException("min");
		}
		int year = Integer.parseInt(atts.getValue(yearIdx));
        int monthOfYear = Integer.parseInt(atts.getValue(monthIdx));
        int dayOfMonth = Integer.parseInt(atts.getValue(dayIdx));
        int hourOfDay = Integer.parseInt(atts.getValue(hourIdx));
        int minuteOfHour = Integer.parseInt(atts.getValue(minIdx));
        int secondOfMinute = Integer.parseInt(atts.getValue(secIdx));
        this.time = new GregorianCalendar(year, monthOfYear, dayOfMonth, hourOfDay, minuteOfHour, secondOfMinute);
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
