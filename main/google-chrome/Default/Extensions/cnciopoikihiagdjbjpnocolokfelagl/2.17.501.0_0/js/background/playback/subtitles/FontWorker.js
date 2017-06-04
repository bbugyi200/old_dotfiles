importScripts('../../../lib/async.js', '../../../lib/opentype.js');

var FontWorker = function() {
	var self = this;
	webkitRequestFileSystem(0, 16 * 1024 * 1024, function(_fs) {
		self.fs = _fs;
	}, function() {
		console.log('filesystemfail');
	});
};

FontWorker.prototype.getFontCSS = function(directory, cb) {

	var fontFileCSS = '';
	var fontInfoList = [];

	this._getFontFileList(directory, function(err, fileEntryList) {
		async.forEach(fileEntryList, function(fileEntry, next) {

			FontWorker._processFontFileEntry(fileEntry, function(err, css, info) {
				if (!err && css) {
					fontFileCSS += css;
					fontInfoList.push(info);
				}

				next();
			});
		}, function(err) {
			cb(err, fontFileCSS, fontInfoList);
		});
	});
};



FontWorker.prototype._getFontFileList = function(directory, cb) {
  	this.fs.root.getDirectory("/fonts/" + directory, {}, function(directory) {
	  	if (directory) {
	  		var fsReader = directory.createReader();
	  		fsReader.readEntries(function(fileEntryList) {
				if (fileEntryList) {
					cb(null, fileEntryList);
				} else {
					cb('readEntriesFail');
				}
			});
	  	} else {
	  		cb(null, []);
	  	}
 	});

};

FontWorker._processFontFileEntry = function(fileEntry, cb) {
	FontWorker._getFontInfoFromFileEntry(fileEntry, function(err, fontInfo) {

		if (!err && fontInfo) {
			// fontInfo: {
			//  fileName: '',
			//  nameTable: {}
			// }
			var fontWeight = 'normal';
			var fontStyle = 'normal';

			var subFamily = fontInfo.nameTable.fontSubfamily.toLowerCase();

			switch(subFamily) {
				case 'bold italic':
					fontWeight = 'bold';
					fontStyle = 'italic';
				break;

				case 'bold':
				case 'black':
					fontWeight = 'bold';
				break;

				case 'italic':
					fontStyle = 'italic';
				break;

				case 'oblique':
					fontStyle = 'oblique';
				break;
			}
			var css =
				"@font-face { \n" +
					"font-family: '" + fontInfo.nameTable.fontFamily + "';\n" +
					"src: url('/font?name=" + fontInfo.fileName + "') format('truetype');\n" +
					"font-weight: " + fontWeight + ";\n" +
					"font-style: " + fontStyle + ";\n" +
				"}\n\n";

			cb(null, css, {
				fontFamily: fontInfo.nameTable.fontFamily,
				fontWeight: fontWeight,
				fontStyle: fontStyle
			});
		} else {
			cb(err);
		}
	});
};


FontWorker._getFontInfoFromFileEntry = function(fileEntry, cb) {
	fileEntry.file(function(file) {
		var url = URL.createObjectURL(file);
		opentype.load(url, function(err, font) {
			if (!err && font && font.tables && font.tables.name) {
				cb(null, {
					fileName: file.name,
					nameTable: font.tables.name
				});
			} else {
				cb(err || 'noFontInfo');
			}
		});
	});
};



var fontWorker = new FontWorker();
this.onmessage = function(event) {
	//console.log('FontWorker::onmessage', event.data.id, event.data.directory);
	fontWorker.getFontCSS(event.data.directory, function(err, fontFileCSS, fontInfoList) {
		//console.log('FontWorker::postMessage', event.data.id, event.data.directory);
		postMessage({
			id: event.data.id,
			fontFileCSS: fontFileCSS,
			fontInfoList: fontInfoList
		});
	});
};
