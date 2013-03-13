
use strict;
use warnings;

use Module::Build 0.3601;


my %module_build_args = (
  "build_requires" => {
    "File::Find" => 0,
    "File::Temp" => 0,
    "FindBin" => 0,
    "Module::Build" => "0.4003",
    "Path::Tiny" => 0,
    "Test::Fatal" => 0,
    "Test::More" => "0.98",
    "Test::Output" => 0
  },
  "configure_requires" => {
    "Module::Build" => "0.4003"
  },
  "dist_abstract" => "A collection of Gentoo::Overlay objects.",
  "dist_author" => [
    "Kent Fredric <kentnl\@cpan.org>"
  ],
  "dist_name" => "Gentoo-Overlay-Group",
  "dist_version" => "0.2.0",
  "license" => "perl",
  "module_name" => "Gentoo::Overlay::Group",
  "recommends" => {},
  "recursive_test_files" => 1,
  "requires" => {
    "Gentoo::Overlay" => "v1.0.3",
    "Gentoo::Overlay::Exceptions" => 0,
    "Gentoo::Overlay::Types" => 0,
    "Moose" => 0,
    "MooseX::Has::Sugar" => 0,
    "MooseX::Types::Moose" => 0,
    "MooseX::Types::Path::Tiny" => 0,
    "Scalar::Util" => 0,
    "namespace::autoclean" => 0,
    "perl" => "v5.10.0",
    "strict" => 0,
    "warnings" => 0
  },
  "script_files" => []
);


my $build = Module::Build->new(%module_build_args);

$build->create_build_script;
