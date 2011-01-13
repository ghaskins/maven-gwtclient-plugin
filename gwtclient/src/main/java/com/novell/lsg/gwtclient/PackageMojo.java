package com.novell.lsg.gwtclient;

/*
 * Copyright 2001-2005 The Apache Software Foundation.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.compress.archivers.tar.TarArchiveEntry;
import org.apache.commons.compress.archivers.tar.TarArchiveOutputStream;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;

/**
 * Goal which packages a GWT client into an archive
 * 
 * @goal package
 * 
 * @phase package
 */
public class PackageMojo extends AbstractMojo {

	/**
	 * The directory to archive
	 * 
	 * @parameter expression="${project.build.directory}/${project.build.finalName}"
	 * @required
	 */
	private File m_directory;
	
	/**
	 * Location of the file.
	 * 
	 * @parameter expression="${project.build.directory}"
	 * @required
	 */
	private File m_outputDirectory;

	/**
	 * Name of archive
	 * 
	 * @parameter expression="${project.build.finalName}.gwtclient"
	 * @required
	 */
	private String m_archiveName;
	
	public void execute() throws MojoExecutionException {
		validateDirectory();
		List<File> files = getFileListing(m_directory);
		
		File f = m_outputDirectory;

		if (!f.exists()) {
			f.mkdirs();
		}

		File archive = new File(f, m_archiveName);

		FileOutputStream os = null;
		TarArchiveOutputStream tar = null;
		try {
			byte[] buf = new byte[4 * 1024];
			
			os = new FileOutputStream(archive);
			tar = new TarArchiveOutputStream(os);
	
			for(File file : files) {
				String          localname = file.getAbsolutePath().substring(m_directory.toString().length()+1);
				TarArchiveEntry entry     = new TarArchiveEntry(file, localname);
				
				getLog().debug("adding " + localname);
				
				tar.putArchiveEntry(entry);
				
				if (file.isFile() && file.length() != 0) {
					FileInputStream is = null;
					
					try {
						int bytesread;
						
						is  = new FileInputStream(file);
					
						while((bytesread = is.read(buf)) != -1) {
							tar.write(buf, 0, bytesread);
						}
					} catch (Exception e) {
						throw new MojoExecutionException("Error reading file + " );
					} finally {
						is.close();
					}
				}
				
				tar.closeArchiveEntry();
			}
		} catch (IOException e) {
			throw new MojoExecutionException("Error creating file " + archive, e);
		} finally {
			if (tar != null) {
				try {
					tar.flush();
				} catch (IOException e) {
					// ignore
				}
			}
			
			if (os != null) {
				try {
					os.close();
				} catch (IOException e) {
					// ignore
				}
			}
		}
	}

	static private List<File> getFileListing(File path)
			throws MojoExecutionException {
		List<File> result = new ArrayList<File>();
		File[] filesAndDirs = path.listFiles();
		List<File> filesDirs = Arrays.asList(filesAndDirs);
		for (File file : filesDirs) {
			result.add(file); // always add, even if directory
			if (!file.isFile()) {
				// must be a directory
				// recursive call!
				List<File> deeperList = getFileListing(file);
				result.addAll(deeperList);
			}
		}
		return result;
	}

	/**
	 * Directory is valid if it exists, does not represent a file, and can be
	 * read.
	 */
	private void validateDirectory()
			throws MojoExecutionException {
		if (m_directory == null) {
			throw new IllegalArgumentException("Directory should not be null.");
		}
		if (!m_directory.exists()) {
			throw new MojoExecutionException("Directory does not exist: "
					+ m_directory);
		}
		if (!m_directory.isDirectory()) {
			throw new IllegalArgumentException("Is not a directory: "
					+ m_directory);
		}
		if (!m_directory.canRead()) {
			throw new IllegalArgumentException("Directory cannot be read: "
					+ m_directory);
		}
	}

}
