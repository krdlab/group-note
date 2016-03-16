'use strict';
angular
  .module('groupNoteApp', [
    'ngAnimate',
    'ngAria',
    'ngCookies',
    'ngMessages',
    'ngResource',
    'ngRoute',
    'ngSanitize',
    'ngTouch',
    'hc.marked',
    'emoji',
    'ui.router',
    'restangular'
  ])
  .config(function ($stateProvider, $urlRouterProvider) {
    $urlRouterProvider.otherwise('/login');
    $stateProvider
      .state('login', {
        url: '/login',
        templateUrl: 'views/login.html',
        controller: 'LoginCtrl',
        requireAuth: false
      })
      .state('register', {
        url: '/register',
        templateUrl: 'views/register.html',
        controller: 'RegisterCtrl',
        requireAuth: false
      })
      .state('main', {
        url: '/',
        templateUrl: 'views/main.html',
        controller: 'MainCtrl',
        requireAuth: true
      })
      .state('main.groups', {
        url: 'groups',
        templateUrl: 'views/main/groups.html',
        controller: 'GroupsCtrl',
        requireAuth: true
      })
      .state('main.groups-detail', {
        url: 'groups/:groupId',
        templateUrl: 'views/main/groups/detail.html',
        controller: 'GroupsDetailCtrl',
        requireAuth: true
      })
      .state('main.notes', {
        url: 'notes',
        templateUrl: 'views/main/notes.html',
        controller: 'NotesCtrl',
        requireAuth: true
      })
      .state('main.notes-new', {
        url: 'notes/new',
        templateUrl: 'views/main/notes/new.html',
        controller: 'NotesNewCtrl',
        requireAuth: true
      })
      .state('main.notes-detail', {
        url: 'notes/:noteId',
        templateUrl: 'views/main/notes/detail.html',
        controller: 'NotesDetailCtrl',
        requireAuth: true
      });
  })
  .config(['markedProvider', function(markedProvider) {
    markedProvider.setOptions({
      gfm: true,
      tables: true,
      highlight: function(code, lang) {
        if (lang) {
          return hljs.highlight(lang, code, true).value;
        } else {
          return hljs.highlightAuto(code).value;
        }
      }
    });
  }])
  .config([
    'RestangularProvider',
    'ApiConstant',
    function(RestangularProvider, ApiConstant) {
      RestangularProvider.setBaseUrl(ApiConstant.base);
    }
  ])
  .run([
    '$rootScope',
    '$state',
    'UserModel',
    function($rootScope, $state, UserModel) {
      $rootScope.$on(
        '$stateChangeStart',
        function(e, toState, toParams, fromState, fromParams) {
          if (!toState.requireAuth) {
            return;
          }
          if (!UserModel.isLoggedIn()) {
            e.preventDefault();
            UserModel.acquireToken()
              .then(function() {
                $state.go(toState.name);
              }, function() {
                $state.go('login');
              });
          }
        }
      );
    }
  ])
;
