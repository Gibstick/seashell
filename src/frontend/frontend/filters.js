/**
 * Copyright (C) 2013-2015 The Seashell Maintainers.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * See also 'ADDITIONAL TERMS' at the end of the included LICENSE file.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/* jshint supernew: true */

/*
 * Comparison function that trims the prefix from Assignment, Lecture, and Tutorial names
 * and provides a numeric comparison for them.
 * Requires an assignment name of the form (A|Lec|Tut)[0-9]+
 * @param {String} a - The first project name to compare
 * @param {String} b - The second project name to compare
 * @returns {Boolean} 1 if a < b; -1 if b < a; 0 if a == b
 */
function trimAndCompare(a, b) {
  // parseInt stops when it encounters non-number, so we don't need to filter trailing junk
  var aNumeric = parseInt(a.slice(a.split(/[0-9]/)[0].length));
  var bNumeric = parseInt(b.slice(b.split(/[0-9]/)[0].length));
  // comparison function follows strcmp convention
  return aNumeric < bNumeric ? -1 : bNumeric < aNumeric ? 1 : 0;

  // type is one of 'A', 'TUT', 'LEC', or 'NONE'
}
angular.module('frontend-app')
  .filter('projectFilter', function() {
    return function(input, type) {
      var pattAssn = new RegExp('^A[0-9]+$');
      var pattTut = new RegExp('^Tut([0-9]+|[0-9]+Sol)$');
      var pattLec = new RegExp('^Lec[0-9]+$');
      var out = [];
      var pattern;
      var isNone = false;
      switch (type) {
        case 'A':
          pattern = pattAssn;
          break;
        case 'LEC':
          pattern = pattLec;
          break;
        case 'TUT':
          pattern = pattTut;
          break;
        case 'NONE':
          isNone = true;
      }

      for (var i = 0; i < input.length; i++) {
        var name = input[i][0];
        if (!isNone && pattern.test(name)) {
          out.push(name);
        } else if (isNone && !pattAssn.test(name) && !pattTut.test(name) && !pattLec.test(name)) {
          out.push(name);
        }
      }

      if (!isNone) out.sort(trimAndCompare);

      return out;
    };
  });
