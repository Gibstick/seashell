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


/**
 * Returns a comparison function that strips the first n characters from two string inputs
 * and compares the rest numerically 
 * @param {Number} n - The non-negative integer number of characters to strip off before comparing
 * @returns {function} The comparison function
 */
function trimAndCompare(n) {
    return function (a, b) {
        // parseInt stops when it encounters non-number, so we don't need to filter trailing junk
        var aNumeric = parseInt(a.slice(n)); 
        var bNumeric = parseInt(b.slice(n));
        // comparison function follows strcmp convention
        return aNumeric < bNumeric ? -1 : bNumeric < aNumeric ? 1 : 0;
    };
}

angular.module('frontend-app')
  .filter('projectFilter', function() {
    return function(input, type){
      var pattAssn = new RegExp('^A[0-9]+$');
      var pattTut = new RegExp('^Tut([0-9]+|[0-9]+Sol)$');
      var pattLec = new RegExp('^Lec[0-9]+$');
      var out = [];
      for(var i = 0; i < input.length; i++){
        if(type === 'A'){
          if(pattAssn.test(input[i][0])){
            out.push(input[i][0]);
          }
        }
        else if(type === 'TUT'){
          if(pattTut.test(input[i][0])){
            out.push(input[i][0]);
          }
        }
        else if(type === 'LEC'){
          if(pattLec.test(input[i][0])){
            out.push(input[i][0]);
          }
        }
        else {
          if(!pattAssn.test(input[i][0]) && !pattTut.test(input[i][0]) && !pattLec.test(input[i][0])){
            out.push(input[i][0]);
          }
        }
      }
      if (type === 'A') {
        out.sort(trimAndCompare(1));        
      } else if (type === 'TUT' || type === 'LEC') {
        out.sort(trimAndCompare(3));
      }
      return out;
    };});
